package modules.admin.UserDashboard;

import java.io.Serializable;

/**
 * Simple POJO to hold the attributes of a UserDashboard Tile.
 */
public class Tile implements Serializable {
	private static final long serialVersionUID = -2095114210805045601L;
	private static final int TILE_TEXT_LENGTH_LIMIT = 50;

	/**
	 * Enumerates tile operations used for CSS and icon selection.
	 */
	@SuppressWarnings("java:S115") // Enum names are dashboard operation CSS/state values.
	public enum Operation {
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

	/**
	 * Fluent builder for immutable-like {@link Tile} creation.
	 */
	public static class Builder {

		private String action;
		private String actionClass;
		private String icon;
		private String link;
		private String operation;
		private String reason;
		private String title;

		/**
		 * Creates an empty builder.
		 */
		public Builder() {
		}

		/**
		 * Sets tile action text.
		 *
		 * @param action The action text.
		 * @return This builder.
		 */
		public Builder action(@SuppressWarnings("hiding") final String action) {
			this.action = action;
			return this;
		}

		/**
		 * Sets the action icon CSS class.
		 *
		 * @param actionClass The icon class.
		 * @return This builder.
		 */
		public Builder actionClass(@SuppressWarnings("hiding") final String actionClass) {
			this.actionClass = actionClass;
			return this;
		}

		/**
		 * Sets the document icon markup.
		 *
		 * @param icon The icon markup.
		 * @return This builder.
		 */
		public Builder icon(@SuppressWarnings("hiding") final String icon) {
			this.icon = icon;
			return this;
		}

		/**
		 * Sets click behaviour by converting a URL into onclick markup.
		 *
		 * @param link The target URL.
		 * @return This builder.
		 */
		public Builder link(@SuppressWarnings("hiding") final String link) {
			if (link != null && (! link.isEmpty())) {
				this.link = String.format("onclick=\"location.href='%s';\"", link);
			}
			return this;
		}

		/**
		 * Sets tile operation.
		 *
		 * @param operation The operation enum.
		 * @return This builder.
		 */
		public Builder operation(@SuppressWarnings("hiding") final Operation operation) {
			this.operation = operation.toString();
			return this;
		}

		/**
		 * Sets explanatory reason text.
		 *
		 * @param reason The reason text.
		 * @return This builder.
		 */
		public Builder reason(@SuppressWarnings("hiding") final String reason) {
			this.reason = reason;
			return this;
		}

		/**
		 * Sets tile title text.
		 *
		 * @param title The tile title.
		 * @return This builder.
		 */
		public Builder title(@SuppressWarnings("hiding") final String title) {
			this.title = title;
			return this;
		}

		/**
		 * Builds a tile instance from current builder state.
		 *
		 * @return A new tile.
		 */
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

	/**
	 * Returns tile action text.
	 *
	 * @return Action text.
	 */
	public String getAction() {
		return action;
	}

	/**
	 * Returns action icon class.
	 *
	 * @return CSS class for action icon.
	 */
	public String getActionClass() {
		return actionClass;
	}

	/**
	 * Returns document icon markup.
	 *
	 * @return Icon markup.
	 */
	public String getIcon() {
		return icon;
	}

	/**
	 * Returns onclick markup link.
	 *
	 * @return Link markup.
	 */
	public String getLink() {
		return link;
	}

	/**
	 * Returns operation style token.
	 *
	 * @return Operation token.
	 */
	public String getOperation() {
		return operation;
	}

	/**
	 * Returns tile reason text.
	 *
	 * @return Reason text.
	 */
	public String getReason() {
		return reason;
	}

	/**
	 * Returns title text truncated to tile display limit.
	 *
	 * @return Display-safe title.
	 */
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

	/**
	 * Returns a hash code derived from tile link identity semantics.
	 *
	 * @return Hash code based on {@code link}.
	 */
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
		} else if (! link.equals(other.link))
			return false;
		return true;
	}
}
