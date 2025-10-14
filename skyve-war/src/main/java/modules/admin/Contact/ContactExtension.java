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
}
