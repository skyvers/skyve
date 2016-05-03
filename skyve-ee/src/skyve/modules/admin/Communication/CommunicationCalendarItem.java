package modules.admin.Communication;

public class CommunicationCalendarItem {

	private String googleCalendarLink;
	private String yahooCalendarLink;
	private byte[] icsFileAttachment;
	
	public String getGoogleCalendarLink() {
		return googleCalendarLink;
	}
	public void setGoogleCalendarLink(String googleCalendarLink) {
		this.googleCalendarLink = googleCalendarLink;
	}
	public String getYahooCalendarLink() {
		return yahooCalendarLink;
	}
	public void setYahooCalendarLink(String yahooCalendarLink) {
		this.yahooCalendarLink = yahooCalendarLink;
	}
	public byte[] getIcsFileAttachment() {
		return icsFileAttachment;
	}
	public void setIcsFileAttachment(byte[] icsFileAttachment) {
		this.icsFileAttachment = icsFileAttachment;
	}
	public CommunicationCalendarItem(String googleCalendarLink, String yahooCalendarLink, byte[] icsFileAttachment) {
		this.googleCalendarLink = googleCalendarLink;
		this.yahooCalendarLink = yahooCalendarLink;
		this.icsFileAttachment = icsFileAttachment;
	}
	public CommunicationCalendarItem() {
	}
	
}
