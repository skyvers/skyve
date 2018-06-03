package org.skyve.nlp.cron.elementprovider.hour;

public class Noon extends Midnight {

	@Override
	public boolean matches(String value) {
		match = value != null &&
				(value.toLowerCase().indexOf("noon") >= 0 || value.toLowerCase().indexOf("midday") >= 0);
		return match;
	}

	@Override
	public String getHourElement() {
		return "12";
	}
}
