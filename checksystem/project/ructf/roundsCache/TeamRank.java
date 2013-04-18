package ructf.roundsCache;

public class TeamRank implements Comparable<TeamRank> {
	
	private int team;
	private Double rank;
	private Double rank_def;
	private Double rank_att;
	private int attackPrice;
	
	public TeamRank(int team, Double rank_def, Double rank_att) {
		this.team = team;
		this.rank_def = rank_def;
		this.rank_att = rank_att;
		rank = rank_def + rank_att;
	}

	public int getTeam() {
		return team;
	}
	
	public Double getRank() {
		return rank;
	}

	public Double getRankDefense() {
		return rank_def;
	}

	public Double getRankAttack() {
		return rank_att;
	}
	
	public int getFlagPrice() {
		return attackPrice;
	}
	
	public void setAttackPrice(int price) {
		attackPrice = price;
	}
	
	public void normalize(Double maxRank) {
		rank = 100*rank/maxRank;
	}
	
	@Override
	public int compareTo(TeamRank o) {
		return rank.compareTo(o.rank);
	}
	
	@Override
	public String toString() {
		return String.format("%d->(%.2f|%d)", team, rank, attackPrice);
	}
	
}
