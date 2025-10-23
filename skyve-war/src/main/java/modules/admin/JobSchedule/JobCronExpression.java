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
 * <p>The class uses reflection to access private fields of the underlying Quartz CronExpression
 * object, enabling detailed examination of cron schedule components that are not exposed through
 * the standard Quartz API.</p>
 * 
 * <p>Example usage:</p>
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

	public JobCronExpression(String cronExpression)
			throws ParseException {
		expression = new CronExpression(cronExpression);
	}

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

	@SuppressWarnings("unchecked")
	public Set<Integer> getMinutes() {
		return (Set<Integer>) get(FIELD_MINUTES);
	}

	@SuppressWarnings("unchecked")
	public Set<Integer> getHours() {
		return (Set<Integer>) get(FIELD_HOURS);
	}

	@SuppressWarnings("unchecked")
	public Set<Integer> getDaysOfMonth() {
		return (Set<Integer>) get(FIELD_DAYS_OF_MONTH);
	}

	@SuppressWarnings("unchecked")
	public Set<Integer> getMonths() {
		return (Set<Integer>) get(FIELD_MONTHS);
	}

	@SuppressWarnings("unchecked")
	public Set<Integer> getDaysOfWeek() {
		return (Set<Integer>) get(FIELD_DAYS_OF_WEEK);
	}

	public boolean getLastDayOfWeek() {
		return Boolean.TRUE.equals(get(FIELD_LAST_DAY_OF_WEEK));
	}

	public boolean getLastDayOfMonth() {
		return Boolean.TRUE.equals(get(FIELD_LAST_DAY_OF_MONTH));
	}

	public boolean getNearestWeekday() {
		return Boolean.TRUE.equals(get(FIELD_NEAREST_WEEKDAY));
	}
}
