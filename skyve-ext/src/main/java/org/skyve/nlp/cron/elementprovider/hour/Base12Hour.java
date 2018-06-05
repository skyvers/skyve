package org.skyve.nlp.cron.elementprovider.hour;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Base12Hour extends Base24Hour {

	private static final String PATTERN = "(1[012]|[1-9]):([0-5][0-9])?(?i)\\s?(am|pm)";
	private Pattern pattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);

	@Override
	public boolean matches(String value) {
		Matcher m = pattern.matcher(value);
		while (m.find()) {
			for (int i = 0; i <= m.groupCount(); i++) {
				if (m.group(i) != null) {
					segments.add(m.group(i));
				}
			}
		}
		return segments.size() > 0;
	}

	@Override
	public String getHourElement() {
		if (segments.size() > 3) {
			if (segments.get(3).toLowerCase().equals("pm") && Integer.parseInt(segments.get(1)) < 12) {
				return String.valueOf(Integer.parseInt(segments.get(1)) + 12);
			} else if (segments.get(3).toLowerCase().equals("am") && Integer.parseInt(segments.get(1)) == 12) {
				return "0";
			}
		}
		return segments.size() > 1 ? segments.get(1) : null;
	}
}
