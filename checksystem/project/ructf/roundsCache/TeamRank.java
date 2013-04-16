package ructf.roundsCache;

public class TeamRank implements Comparable<TeamRank> {
	
	private int team;
	private Double rank;
	private int attackPrice;
	
	public TeamRank(int team, Double rank) {
		this.team = team;
		this.rank = rank;
	}

	public int getTeam() {
		return team;
	}
	
	public Double getRank() {
		return rank;
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
		return String.format("%d->(%.2f,%d)", team, rank, attackPrice);
	}
	
}
