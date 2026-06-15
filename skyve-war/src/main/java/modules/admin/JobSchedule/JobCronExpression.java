package modules.admin.JobSchedule;

import java.lang.reflect.Field;
import java.text.ParseException;
import java.util.Set;

import org.quartz.CronExpression;
import org.skyve.domain.messages.DomainException;

/**
 * A wrapper class for Quartz CronExpression that provides access to internal scheduling components
 * using reflection. This class allows inspection of the parsed cron expression fields such as
 * minutes, hours, days, months, and special scheduling flags.
 * 
 * <p>
 * The class uses reflection to access private fields of the underlying Quartz CronExpression
 * object, enabling detailed examination of cron schedule components that are not exposed through
 * the standard Quartz API.
 * </p>
 * 
 * <p>
 * Example usage:
 * </p>
 * 
 * <pre>
 * JobCronExpression cronExpr = new JobCronExpression("0 15 10 * * ?");
 * Set&lt;Integer&gt; hours = cronExpr.getHours(); // Returns {15}
 * Set&lt;Integer&gt; minutes = cronExpr.getMinutes(); // Returns {0}
 * </pre>
 * 
 * @author Skyve Framework
 * @see org.quartz.CronExpression
 */
public class JobCronExpression {
	// Field name constants for reflection access
	private static final String FIELD_MINUTES = "minutes";
	private static final String FIELD_HOURS = "hours";
	private static final String FIELD_DAYS_OF_MONTH = "daysOfMonth";
	private static final String FIELD_MONTHS = "months";
	private static final String FIELD_DAYS_OF_WEEK = "daysOfWeek";
	private static final String FIELD_LAST_DAY_OF_WEEK = "lastdayOfWeek";
	private static final String FIELD_LAST_DAY_OF_MONTH = "lastdayOfMonth";
	private static final String FIELD_NEAREST_WEEKDAY = "nearestWeekday";

	private CronExpression expression = null;

	/**
	 * Parses a Quartz cron expression for reflective field inspection.
	 *
	 * @param cronExpression
	 *        the cron expression to parse
	 * @throws ParseException
	 *         if the expression is invalid
	 */
	public JobCronExpression(String cronExpression)
			throws ParseException {
		expression = new CronExpression(cronExpression);
	}

	/**
	 * Retrieves a private field from Quartz {@link CronExpression} state.
	 *
	 * @param name
	 *        the private field name
	 * @return the reflective field value
	 */
	private Object get(String name) {
		try {
			Field f = expression.getClass().getDeclaredField(name);
			if (!f.canAccess(expression)) {
				f.setAccessible(true);
			}
			return f.get(expression);
		} catch (Exception e) {
			throw new DomainException("Cant get " + name + " from CRON expression " + expression.getCronExpression(), e);
		}
	}

	/**
	 * Returns configured minute values.
	 *
	 * @return the minute set from the parsed expression
	 */
	@SuppressWarnings("unchecked")
	public Set<Integer> getMinutes() {
		return (Set<Integer>) get(FIELD_MINUTES);
	}

	/**
	 * Returns configured hour values.
	 *
	 * @return the hour set from the parsed expression
	 */
	@SuppressWarnings("unchecked")
	public Set<Integer> getHours() {
		return (Set<Integer>) get(FIELD_HOURS);
	}

	/**
	 * Returns configured day-of-month values.
	 *
	 * @return the day-of-month set from the parsed expression
	 */
	@SuppressWarnings("unchecked")
	public Set<Integer> getDaysOfMonth() {
		return (Set<Integer>) get(FIELD_DAYS_OF_MONTH);
	}

	/**
	 * Returns configured month values.
	 *
	 * @return the month set from the parsed expression
	 */
	@SuppressWarnings("unchecked")
	public Set<Integer> getMonths() {
		return (Set<Integer>) get(FIELD_MONTHS);
	}

	/**
	 * Returns configured day-of-week values.
	 *
	 * @return the day-of-week set from the parsed expression
	 */
	@SuppressWarnings("unchecked")
	public Set<Integer> getDaysOfWeek() {
		return (Set<Integer>) get(FIELD_DAYS_OF_WEEK);
	}

	/**
	 * Indicates whether the expression uses the last day-of-week token.
	 *
	 * @return {@code true} when last day-of-week is enabled
	 */
	public boolean getLastDayOfWeek() {
		return Boolean.TRUE.equals(get(FIELD_LAST_DAY_OF_WEEK));
	}

	/**
	 * Indicates whether the expression uses the last day-of-month token.
	 *
	 * @return {@code true} when last day-of-month is enabled
	 */
	public boolean getLastDayOfMonth() {
		return Boolean.TRUE.equals(get(FIELD_LAST_DAY_OF_MONTH));
	}

	/**
	 * Indicates whether the expression uses nearest-weekday semantics.
	 *
	 * @return {@code true} when nearest-weekday is enabled
	 */
	public boolean getNearestWeekday() {
		return Boolean.TRUE.equals(get(FIELD_NEAREST_WEEKDAY));
	}
}
