package org.skyve.nlp.cron;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.elementprovider.DayNumber;
import org.skyve.nlp.cron.elementprovider.hour.Base12Hour;
import org.skyve.nlp.cron.elementprovider.hour.Base12HourShort;
import org.skyve.nlp.cron.elementprovider.hour.Base24Hour;
import org.skyve.nlp.cron.elementprovider.hour.Midnight;
import org.skyve.nlp.cron.elementprovider.hour.Noon;
import org.skyve.nlp.cron.elementprovider.recurring.EveryDay;
import org.skyve.nlp.cron.elementprovider.recurring.EveryDayNumber;
import org.skyve.nlp.cron.elementprovider.recurring.EveryDayNumberStep;
import org.skyve.nlp.cron.elementprovider.recurring.EveryHour;
import org.skyve.nlp.cron.elementprovider.recurring.EveryMinute;
import org.skyve.nlp.cron.elementprovider.recurring.EveryMonth;
import org.skyve.nlp.cron.elementprovider.recurring.EveryMonthStep;
import org.skyve.nlp.cron.elementprovider.recurring.EveryWeek;
import org.skyve.nlp.cron.elementprovider.recurring.EveryYear;

/**
 * Natural Cron Expression Parser converted and enhanced Java implementation of
 * https://github.com/bpolaszek/natural-cron-expression.
 * License: MIT
 */
public class NaturalCronExpressionParser {

	public static final String VALID_PATTERN = "^(((?:[1-9]?\\d|\\*)\\s*(?:(?:[\\/-][1-9]?\\d)|(?:,[1-9]?\\d)+)?\\s*){6})$";

	protected ExpressionElementProvider[] elementProviders;
	private Map<String, CronExpression> mappings;
	private Pattern pattern;

	public NaturalCronExpressionParser() {
		elementProviders = new ExpressionElementProvider[] {
				new EveryYear(),
				new EveryMonth(),
				new EveryMonthStep(),
				new EveryWeek(),
				new EveryDayNumber(),
				new EveryDayNumberStep(),
				new EveryDay(),
				new EveryHour(),
				new EveryMinute(),
				new DayNumber(),
				new Noon(),
				new Midnight(),
				new Base12Hour(),
				new Base12HourShort(),
				new Base24Hour(),
		};

		mappings = new HashMap<>();
		mappings.put("yearly", new CronExpression("0", "0", "0", "1", "1", "*"));
		mappings.put("annually", new CronExpression("0", "0", "0", "1", "1", "*"));
		mappings.put("monthly", new CronExpression("0", "0", "0", "1", "*", "*"));
		mappings.put("weekly", new CronExpression("0", "0", "0", "*", "*", "0"));
		mappings.put("midnight", new CronExpression("0", "0", "0", "*", "*", "*"));
		mappings.put("daily", new CronExpression("0", "0", "0", "*", "*", "*"));
		mappings.put("hourly", new CronExpression("0", "0", "*", "*", "*", "*"));
		mappings.put("midday", new CronExpression("0", "0", "12", "*", "*", "*"));

		pattern = Pattern.compile(VALID_PATTERN);
	}

	public CronExpression parse(final String string) {

		if (string != null) {
			String e = string.toLowerCase();

			if (mappings.get(e) != null) {
				return mappings.get(e);
			}

			CronExpression ce = new CronExpression();
			boolean isSecondElementLocked = false,
					isMinuteElementLocked = false,
					isHourElementLocked = false,
					isDayNumberElementLocked = false,
					isMonthElementLocked = false,
					isDayOfWeekElementLocked = false;

			for (ExpressionElementProvider elementProvider : elementProviders) {
				if (elementProvider.matches(string)) {
					if (shouldUpdateSecond(elementProvider, isSecondElementLocked)) {
						ce.setSecond(elementProvider.getSecondElement());
					}

					if (shouldUpdateMinute(elementProvider, isMinuteElementLocked)) {
						ce.setMinute(elementProvider.getMinuteElement());
					}

					if (shouldUpdateHour(elementProvider, isHourElementLocked)) {
						ce.setHour(elementProvider.getHourElement());
					}

					if (shouldUpdateDayNumber(elementProvider, isDayNumberElementLocked)) {
						ce.setDayNumber(elementProvider.getDayNumberElement());
					}

					if (shouldUpdateMonth(elementProvider, isMonthElementLocked)) {
						ce.setMonth(elementProvider.getMonthElement());
					}

					if (shouldUpdateDayOfWeek(elementProvider, isDayOfWeekElementLocked)) {
						ce.setDayOfWeek(elementProvider.getDayOfWeekElement());
					}

					if (elementProvider.isSecondElementLocked()) {
						isSecondElementLocked = true;
					}

					if (elementProvider.isMinuteElementLocked()) {
						isMinuteElementLocked = true;
					}

					if (elementProvider.isHourElementLocked()) {
						isHourElementLocked = true;
					}

					if (elementProvider.isDayNumberElementLocked()) {
						isDayNumberElementLocked = true;
					}

					if (elementProvider.isMonthElementLocked()) {
						isMonthElementLocked = true;
					}

					if (elementProvider.isDayOfWeekElementLocked()) {
						isDayOfWeekElementLocked = true;
					}
				}
			}

			Matcher m = pattern.matcher(ce.toString());
			if (ce.hasNothing() || !m.matches()) {
				throw new CronParserException(String.format("Unable to parse \"%s\"", string));
			}

			return ce;
		}

		return null;
	}

	private static boolean shouldUpdateSecond(ExpressionElementProvider subParser, boolean isSecondElementLocked) {
		return subParser.canProvideSecond() && !isSecondElementLocked;
	}

	private static boolean shouldUpdateMinute(ExpressionElementProvider subParser, boolean isMinuteElementLocked) {
		return subParser.canProvideMinute() && !isMinuteElementLocked;
	}

	private static boolean shouldUpdateHour(ExpressionElementProvider subParser, boolean isHourElementLocked) {
		return subParser.canProvideHour() && !isHourElementLocked;
	}

	private static boolean shouldUpdateDayNumber(ExpressionElementProvider subParser, boolean isDayNumberElementLocked) {
		return subParser.canProvideDayNumber() && !isDayNumberElementLocked;
	}

	private static boolean shouldUpdateMonth(ExpressionElementProvider subParser, boolean isMonthElementLocked) {
		return subParser.canProvideMonth() && !isMonthElementLocked;
	}

	private static boolean shouldUpdateDayOfWeek(ExpressionElementProvider subParser, boolean isDayOfWeekElementLocked) {
		return subParser.canProvideDayOfWeek() && !isDayOfWeekElementLocked;
	}
}
