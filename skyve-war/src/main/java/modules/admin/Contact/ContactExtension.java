package modules.admin.Contact;

import modules.admin.domain.Contact;

public class ContactExtension extends Contact {
	private static final long serialVersionUID = -2378327647462703888L;

	public String bizKey() {
		StringBuilder result = new StringBuilder(64);

		String name = getName();
		result.append((name == null) ? "Unnamed Contact" : name);

		ContactType type = getContactType();
		if (type != null) {
			result.append(" (").append(type).append(')');
		}

		String mobile = getMobile();
		if (mobile != null) {
			result.append(" (m) ").append(mobile);
		}

		return result.toString();
	}

	/**
	 * Returns true if the contact holds no data and can be dispensed with.
	 * 
	 * @param c
	 */
	public static boolean isNothing(Contact c) {
		boolean result = true;

		result = result && (c.getName() == null);
		result = result && (c.getMobile() == null);
		result = result && (c.getEmail1() == null);

		return result;
	}
}
