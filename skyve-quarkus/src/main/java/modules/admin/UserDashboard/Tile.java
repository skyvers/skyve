package modules.admin.UserDashboard;

import java.io.Serializable;

/**
 * Simple POJO to hold the attributes of a UserDashboard Tile.
 */
public class Tile implements Serializable {

	private static final long serialVersionUID = -2095114210805045601L;
	private static final int TILE_TEXT_LENGTH_LIMIT = 50;

	public static enum Operation {
		insert, update, delete, view
	}

	private String action;
	private String actionClass;
	private String icon;
	private String link;
	private String operation;
	private String reason;
	private String title;

	/**
	 * The HTML template which is String formatted to produce the markup.
	 * 
	 * The numbered format paramters correspond to:
	 * 
	 * 1 - update/create/view
	 * 2 - link
	 * 3 - document icon / content image
	 * 4 - action icon
	 * 5 - title
	 * 6 - description
	 */
	private String template = "<div class=\"tile %1$s\" %2$s>"
			+ "  <div>"
			+ "    %3$s"
			+ "    <div class=\"title\"><i class=\"fa %4$s\"></i>%5$s</i></a></div>"
			+ "    <div class=\"description\">%6$s</div>"
			+ "  </div>"
			+ "</div>";

	private Tile() {
		// constructor is private, use the Builder
	}

	public static class Builder {

		private String action;
		private String actionClass;
		private String icon;
		private String link;
		private String operation;
		private String reason;
		private String title;

		public Builder() {
		}

		public Builder action(@SuppressWarnings("hiding") final String action) {
			this.action = action;
			return this;
		}

		public Builder actionClass(@SuppressWarnings("hiding") final String actionClass) {
			this.actionClass = actionClass;
			return this;
		}

		public Builder icon(@SuppressWarnings("hiding") final String icon) {
			this.icon = icon;
			return this;
		}

		public Builder link(@SuppressWarnings("hiding") final String link) {
			if (link != null && link.length() > 0) {
				this.link = String.format("onclick=\"location.href='%s';\"", link);
			}
			return this;
		}

		public Builder operation(@SuppressWarnings("hiding") final Operation operation) {
			this.operation = operation.toString();
			return this;
		}

		public Builder reason(@SuppressWarnings("hiding") final String reason) {
			this.reason = reason;
			return this;
		}

		public Builder title(@SuppressWarnings("hiding") final String title) {
			this.title = title;
			return this;
		}

		public synchronized Tile build() {
			Tile tile = new Tile();
			tile.action = this.action;
			tile.actionClass = this.actionClass;
			tile.icon = this.icon;
			tile.link = this.link;
			tile.operation = this.operation;
			tile.reason = this.reason;
			tile.title = this.title;

			return tile;
		}
	}

	public String getAction() {
		return action;
	}

	public String getActionClass() {
		return actionClass;
	}

	public String getIcon() {
		return icon;
	}

	public String getLink() {
		return link;
	}

	public String getOperation() {
		return operation;
	}

	public String getReason() {
		return reason;
	}

	public String getTitle() {
		return title != null && title.length() > TILE_TEXT_LENGTH_LIMIT
				? title.toString().substring(0, TILE_TEXT_LENGTH_LIMIT - 3) + "..."
				: title;
	}

	/**
	 * Generates the HTML markup for a tile to be presented in the UI.
	 * 
	 * @return A formatted HTML String representation of this tile
	 */
	public String toMarkup() {
		return String.format(template,
				operation,
				link,
				icon,
				actionClass,
				title,
				reason);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((link == null) ? 0 : link.hashCode());
		return result;
	}

	/**
	 * Two tiles are considered equal if they have the same link
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Tile other = (Tile) obj;
		if (link == null) {
			if (other.link != null)
				return false;
		} else if (!link.equals(other.link))
			return false;
		return true;
	}
}
