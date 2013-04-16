package ructf.roundsCache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class FlagPriceManager {
	
	private static double epsilon = 0.0001d;
	private static int minPrice = 15;
	
	private static PreparedStatement psDel;
	private static PreparedStatement psIns;
	private static PreparedStatement psSel;
	private static PreparedStatement psUpd;
	
	public FlagPriceManager(Connection conn) throws SQLException {
		psDel = conn.prepareStatement("DELETE FROM flag_price WHERE round = ?");											// Очистить старые данные 
		psIns = conn.prepareStatement("INSERT INTO flag_price(round,time,team,price,rank,rank_def,rank_att) VALUES (?,NOW(),?,?,?,?,?)");
		psSel = conn.prepareStatement("SELECT team_id, privacy+availability, attack FROM rounds_cache WHERE round = ?");	// Прочитать актуальные данные о баллах
		psUpd = conn.prepareStatement("UPDATE teams SET flag_price = ? WHERE id = ?");										// Обновить цену флага в таблице команд
	}
	
	public void updatePrices(int round) throws SQLException {
		List<TeamDefenseAttack> data = loadDefenseAttack(round);	// load from DB
		ArrayList<TeamRank> ranks = calculateRanks(data);
		Collections.sort(ranks);
		calculatePrices(ranks);
		debugPrintPrices(ranks, round);
		updateDbPrices(ranks, round);
	}
	
	private List<TeamDefenseAttack> loadDefenseAttack(int round) throws SQLException {
		List<TeamDefenseAttack> result = new LinkedList<TeamDefenseAttack>();
		psSel.setInt(1, round);
		ResultSet rs = psSel.executeQuery();
		while (rs.next())
			result.add(new TeamDefenseAttack(rs.getInt(1), rs.getInt(2), rs.getInt(3)));
		return result;
	}

	private void debugPrintPrices(ArrayList<TeamRank> ranks, int round) {
		StringBuilder sb = new StringBuilder();
		for (TeamRank r : ranks)
			sb.append(r.toString() + "  ");
		System.out.println(String.format("FlagPriceManager: round: %d, team->(rank|price): %s", round, sb.toString()));
	}

	private void updateDbPrices(ArrayList<TeamRank> ranks, int round) throws SQLException {
		psDel.setInt(1, round);
		psDel.executeUpdate();
		psIns.setInt(1, round);	// round
		for (TeamRank r : ranks) {
			psIns.setInt(2, r.getTeam());		// team
			psIns.setInt(3, r.getFlagPrice());	// price
			psIns.setDouble(4, r.getRank());
			psIns.setDouble(5, r.getRankDefense());
			psIns.setDouble(6, r.getRankAttack());
			psIns.executeUpdate();
			psUpd.setInt(1, r.getFlagPrice());
			psUpd.setInt(2, r.getTeam());
			psUpd.executeUpdate();
		}
	}
	
	private void calculatePrices(ArrayList<TeamRank> ranks) {
		int price = minPrice;
		int add = 1;
		Double prevRank = -1d;
		for (TeamRank rank : ranks) {
			if (prevRank >= 0) {	// not first element
				if (Math.abs(rank.getRank() - prevRank) > epsilon) { 
					price += add;
					add = 1;
				}
				else 
					add++;
			}
			rank.setAttackPrice(price);
			prevRank = rank.getRank();
		}
	}
	
	private static int getMaxDefense(List<TeamDefenseAttack> data) {
		int max = 0;
		for (TeamDefenseAttack i : data)
			max = Math.max(max, i.getDefense());
		return max;
	}

	private static int getMaxAttack(List<TeamDefenseAttack> data) {
		int max = 0;
		for (TeamDefenseAttack i : data)
			max = Math.max(max, i.getAttack());
		return max;
	}
	
	private static ArrayList<TeamRank> calculateRanks(List<TeamDefenseAttack> data) {
		ArrayList<TeamRank> ranks = new ArrayList<TeamRank>();
		
		int maxDefense = getMaxDefense(data);
		int maxAttack = getMaxAttack(data);
		
	    Double maxRank = 0d;
	    for (TeamDefenseAttack i : data) {
	    	Double rankDefense = maxDefense > 0 ? (double) (100*i.getDefense())/maxDefense : 0;
	    	Double rankAttack  = maxAttack > 0 ? (double) (100*i.getAttack())/maxAttack : 0;
	    	TeamRank r = new TeamRank(i.getTeam(), rankDefense, rankAttack);
	    	ranks.add(r);
	    	maxRank = Math.max(maxRank, r.getRank());
	    }
	    // Normalize ranks to 100%
	    for (TeamRank rank : ranks)
	    	rank.normalize(maxRank);
		return ranks;
	}
}
