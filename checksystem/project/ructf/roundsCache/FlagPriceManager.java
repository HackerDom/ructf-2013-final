package ructf.roundsCache;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;

public class FlagPriceManager {
	
	private static int minPrice = 15;
	
	private static PreparedStatement psDel;
	private static PreparedStatement psIns;
	
	public FlagPriceManager(Connection conn) throws SQLException {
		psDel = conn.prepareStatement("DELETE FROM flag_price WHERE round = ?");
		psIns = conn.prepareStatement("INSERT INTO flag_price(round,time,team,price) VALUES (?,NOW(),?,?)");
	}
	
	public void updatePrices(int round, Hashtable<Integer, TeamScores> teamsScores) throws SQLException {
		List<Integer> teams = getTeams(teamsScores);
		ArrayList<TeamRank> ranks = calculateRanks(teams, teamsScores);
		Collections.sort(ranks);
		calculatePrices(ranks);
		debugPrintPrices(ranks, round);
		updateDbPrices(ranks, round);
	}
	
	private void debugPrintPrices(ArrayList<TeamRank> ranks, int round) {
		StringBuilder sb = new StringBuilder();
		for (TeamRank r : ranks)
			sb.append(r.toString() + "  ");
		System.out.println(String.format("round: %d, team->(rank,price): %s", round, sb.toString()));
	}

	private void updateDbPrices(ArrayList<TeamRank> ranks, int round) throws SQLException {
		psDel.setInt(1, round);
		psDel.executeUpdate();
		psIns.setInt(1, round);	// round
		for (TeamRank r : ranks) {
			psIns.setInt(2, r.getTeam());		// team
			psIns.setInt(3, r.getFlagPrice());	// price
		}
	}
	
	private void calculatePrices(ArrayList<TeamRank> ranks) {
		int price = minPrice;
		int add = 1;
		Double prevRank = 0d;
		for (TeamRank rank : ranks) {
			rank.setAttackPrice(price);
			if (rank.getRank() != prevRank) { 
				price += add;
				add = 1;
			}
			else 
				add++;
			prevRank = rank.getRank();
		}
	}

	private static List<Integer> getTeams(Hashtable<Integer, TeamScores> teamsScores)
	{
		List<Integer> teams = new ArrayList<>();
	    Enumeration<Integer> e = teamsScores.keys();
	    while (e.hasMoreElements()) {
	    	teams.add(e.nextElement());
	    }
	    return teams;
	}
	
	private static int getMaxDefense(Hashtable<Integer, TeamScores> teamsScores) {
		int max = 0;
		for (TeamScores sc : teamsScores.values()) {
			max = Math.max(max, sc.calculateDefense());
		}
		return max;
	}

	private static int getMaxAttack(Hashtable<Integer, TeamScores> teamsScores) {
		int max = 0;
		for (TeamScores sc : teamsScores.values()) {
			max = Math.max(max, sc.getAttack());
		}
		return max;
	}
	
	private static ArrayList<TeamRank> calculateRanks(List<Integer> teams, Hashtable<Integer, TeamScores> teamsScores) {
		ArrayList<TeamRank> ranks = new ArrayList<TeamRank>();
		
		int maxDefense = getMaxDefense(teamsScores);
		int maxAttack = getMaxAttack(teamsScores);
		
	    Double maxRank = 0d;
	    for (Integer team : teams) {
	    	Double rankDefense = (double) (100*teamsScores.get(team).calculateDefense())/maxDefense;
	    	Double rankAttack  = (double) (100*teamsScores.get(team).getAttack())/maxAttack;
	    	Double rank = rankDefense + rankAttack;
	    	ranks.add(new TeamRank(team, rank));
	    	maxRank = Math.max(maxRank, rank);
	    }
	    // Normalize ranks to 100%
	    for (TeamRank rank : ranks) {
	    	rank.normalize(maxRank);
	    }
		return ranks;
	}
}
