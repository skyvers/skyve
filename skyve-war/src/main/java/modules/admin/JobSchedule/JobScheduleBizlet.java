package modules.admin.JobSchedule;

import java.lang.reflect.Field;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.quartz.CronExpression;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;

import modules.admin.domain.JobSchedule;

public class JobScheduleBizlet extends Bizlet<JobSchedule> {
	public static String getBizKey(JobSchedule schedule) {
		try {
			String jobName = schedule.getJobName();
			int dotIndex = jobName.indexOf('.');
			Customer customer = CORE.getUser().getCustomer();
			Module module = customer.getModule(jobName.substring(0, dotIndex));
			JobMetaData job = module.getJob(jobName.substring(dotIndex + 1));
	
			return module.getName() + " - " + job.getLocalisedDisplayName();
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "";
		}
	}
	
	public static class JobCronExpression {
		private CronExpression expression = null;

		public JobCronExpression(String cronExpression)
		throws ParseException {
			expression = new CronExpression(cronExpression);
		}

		private Object get(String name) {
			try {
				Field f = expression.getClass().getDeclaredField(name);
				if (! f.canAccess(expression)) {
					f.setAccessible(true);
				}
				return f.get(expression);
			}
			catch (Exception e) {
				throw new DomainException("Cant get " + name + " from CRON expression " + expression.getCronExpression(), e);
			}
		}
		
		@SuppressWarnings("unchecked")
		public Set<Integer> getMinutes() {
			return (Set<Integer>) get("minutes");
		}
		
		@SuppressWarnings("unchecked")
		public Set<Integer> getHours() {
			return (Set<Integer>) get("hours");
		}
		
		@SuppressWarnings("unchecked")
		public Set<Integer> getDaysOfMonth() {
			return (Set<Integer>) get("daysOfMonth");
		}

		@SuppressWarnings("unchecked")
		public Set<Integer> getMonths() {
			return (Set<Integer>) get("months");
		}

		@SuppressWarnings("unchecked")
		public Set<Integer> getDaysOfWeek() {
			return (Set<Integer>) get("daysOfWeek");
		}
		
		public boolean getLastDayOfWeek() {
			return Boolean.TRUE.equals(get("lastdayOfWeek"));
		}

		public boolean getLastDayOfMonth() {
			return Boolean.TRUE.equals(get("lastdayOfMonth"));
		}
		public boolean getNearestWeekday() {
			return Boolean.TRUE.equals(get("nearestWeekday"));
		}
	}

	private static final String ALL_CODE = "*";
	private static final Integer ALL_CODE_SPEC = Integer.valueOf(99);
	private static final String SELECTED_CODE = "X";
	private static final String LAST_DAY_CODE = "L";
	private static final String LAST_WEEK_DAY_CODE = "LW";
	private static final String ANY_CODE = "?";
	private static final Integer ANY_CODE_SPEC = Integer.valueOf(98);
	
	@Override
	public JobSchedule newInstance(JobSchedule bean) throws Exception {
		bean.setAllMinutes(ALL_CODE);
		bean.setAllHours(ALL_CODE);
		bean.setAllDays(ALL_CODE);
		bean.setAllMonths(ALL_CODE);
		bean.setAllWeekdays(ALL_CODE);
		
		return bean;
	}

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName)
	throws Exception {
		List<DomainValue> result = null;
		
		if (JobSchedule.allMinutesPropertyName.equals(attributeName) ||
				JobSchedule.allHoursPropertyName.equals(attributeName) ||
				JobSchedule.allMonthsPropertyName.equals(attributeName) ||
				JobSchedule.allWeekdaysPropertyName.equals(attributeName)) {
			result = new ArrayList<>(2);
			result.add(new DomainValue(ALL_CODE, "All"));
			result.add(new DomainValue(SELECTED_CODE, "Selected"));
		}
		else if (JobSchedule.allDaysPropertyName.equals(attributeName)) {
			result = new ArrayList<>(4);
			result.add(new DomainValue(ALL_CODE, "All"));
			result.add(new DomainValue(LAST_DAY_CODE, "Last Day"));
			result.add(new DomainValue(LAST_WEEK_DAY_CODE, "Last Week Day"));
			result.add(new DomainValue(SELECTED_CODE, "Selected"));
		}
		
		return result;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName)
	throws Exception {
		List<DomainValue> result = new ArrayList<>();

		if (JobSchedule.jobNamePropertyName.equals(attributeName)) {
			Customer customer = CORE.getUser().getCustomer();
			
			for (Module module : customer.getModules()) {
				for (JobMetaData job : module.getJobs()) {
					StringBuilder sb = new StringBuilder(128);
					StringBuilder sbDisplay = new StringBuilder(128);
					sb.append(module.getName()).append('.').append(job.getName());
					sbDisplay.append(module.getName()).append(" - ").append(job.getLocalisedDisplayName());
					result.add(new DomainValue(sb.toString(), sbDisplay.toString()));
				}
			}
		}
		
		return result;
	}

	@Override
	public void postLoad(JobSchedule bean) throws Exception {
		JobCronExpression expression = new JobCronExpression(bean.getCronExpression());
		
		Set<Integer> minutes = expression.getMinutes();
		Set<Integer> hours = expression.getHours();
		Set<Integer> days = expression.getDaysOfMonth();
		Set<Integer> months = expression.getMonths();
		Set<Integer> weekdays = expression.getDaysOfWeek();
		
		if (minutes.contains(ALL_CODE_SPEC)) {
			bean.setAllMinutes(ALL_CODE);
		}
		else {
			bean.setAllMinutes(SELECTED_CODE);
			for (int i = 0, l = 60; i < l; i++) {
				Binder.set(bean, "minute" + i, minutes.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
			}
		}

		if (hours.contains(ALL_CODE_SPEC)) {
			bean.setAllHours(ALL_CODE);
		}
		else {
			bean.setAllHours(SELECTED_CODE);
			for (int i = 0, l = 24; i < l; i++) {
				Binder.set(bean, "hour" + i, hours.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
			}
		}
		
		if (days.contains(ALL_CODE_SPEC) | days.contains(ANY_CODE_SPEC)) {
			bean.setAllDays(ALL_CODE);
		}
		else if (expression.getLastDayOfMonth()) {
			if (expression.getNearestWeekday()) {
				bean.setAllDays(LAST_WEEK_DAY_CODE);
			}
			else {
				bean.setAllDays(LAST_DAY_CODE);
			}
		}
		else {
			bean.setAllDays(SELECTED_CODE);
			for (int i = 1, l = 32; i < l; i++) {
				Binder.set(bean, "day" + i, days.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
			}
		}
		
		if (months.contains(ALL_CODE_SPEC)) {
			bean.setAllMonths(ALL_CODE);
		}
		else {
			bean.setAllMonths(SELECTED_CODE);
			for (int i = 1, l = 13; i < l; i++) {
				Binder.set(bean, "month" + i, months.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
			}
		}
		
		if (weekdays.contains(ALL_CODE_SPEC) || weekdays.contains(ANY_CODE_SPEC)) {
			bean.setAllWeekdays(ALL_CODE);
		}
		else {
			bean.setAllWeekdays(SELECTED_CODE);
			for (int i = 1, l = 8; i < l; i++) {
				Binder.set(bean, "weekday" + i, weekdays.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
			}
		}
	}

	@Override
	public void preSave(JobSchedule bean) throws Exception {
		StringBuilder expression = new StringBuilder(128);
		
		// append seconds
		expression.append("0 ");
		
		// append minutes
		if (ALL_CODE.equals(bean.getAllMinutes())) {
			expression.append(ALL_CODE);
		}
		else {
			for (int i = 0, l = 60; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "minute" + i))) {
					expression.append(i).append(',');
				}
			}
			expression.setLength(expression.length() - 1); // remove last comma
		}
		expression.append(' ');

		// append hours
		if (ALL_CODE.equals(bean.getAllHours())) {
			expression.append(ALL_CODE);
		}
		else {
			for (int i = 0, l = 24; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "hour" + i))) {
					expression.append(i).append(',');
				}
			}
			expression.setLength(expression.length() - 1); // remove last comma
		}
		expression.append(' ');
		
		// append days
		String allDays = bean.getAllDays();
		String allWeekdays = bean.getAllWeekdays();
		if (ALL_CODE.equals(allDays)) {
			if (SELECTED_CODE.equals(allWeekdays)) {
				expression.append(ANY_CODE);
			}
			else {
				expression.append(ALL_CODE);
			}
		}
		else if (LAST_DAY_CODE.equals(allDays)) {
			expression.append(LAST_DAY_CODE);
		}
		else if (LAST_WEEK_DAY_CODE.equals(allDays)) {
			expression.append(LAST_WEEK_DAY_CODE);
		}
		else {
			for (int i = 1, l = 32; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "day" + i))) {
					expression.append(i).append(',');
				}
			}
			expression.setLength(expression.length() - 1); // remove last comma
		}
		expression.append(' ');

		// append months
		if (ALL_CODE.equals(bean.getAllMonths())) {
			expression.append(ALL_CODE);
		}
		else {
			for (int i = 1, l = 13; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "month" + i))) {
					expression.append(i).append(',');
				}
			}
			expression.setLength(expression.length() - 1); // remove last comma
		}
		expression.append(' ');
		
		// append weekdays
		if (ALL_CODE.equals(bean.getAllWeekdays())) {
			expression.append(ANY_CODE);
		}
		else {
			for (int i = 1, l = 8; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "weekday" + i))) {
					expression.append(i).append(',');
				}
			}
			expression.setLength(expression.length() - 1); // remove last comma
		}

		bean.setCronExpression(expression.toString());
	}

	/**
	 * Reschedule this job after any runAs user name change has been flushed.
	 */
	@Override
	public void postSave(JobSchedule bean) throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		
		// Re-schedule the job
		JobScheduler jobScheduler = EXT.getJobScheduler();
		jobScheduler.unscheduleJob(bean, customer);
		if (! Boolean.TRUE.equals(bean.getDisabled())) {
			jobScheduler.scheduleJob(bean, bean.getRunAs().toMetaDataUser());
		}
	}
	
	@Override
	public void preDelete(JobSchedule bean) throws Exception {
		EXT.getJobScheduler().unscheduleJob(bean, CORE.getUser().getCustomer());
	}

	@Override
	public void validate(JobSchedule bean, ValidationException e)
	throws Exception {
		if ((! ALL_CODE.equals(bean.getAllDays())) && (! ALL_CODE.equals(bean.getAllWeekdays()))) {
			e.getMessages().add(new Message(new String[] {JobSchedule.allDaysPropertyName, JobSchedule.allWeekdaysPropertyName},
												"Choose week days or days of the month, but not both"));
		}
		
		if (SELECTED_CODE.equals(bean.getAllMinutes())) {
			boolean found = false;
			for (int i = 0, l = 60; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "minute" + i))) {
					found = true;
					break;
				}
			}
			if (! found) {
				e.getMessages().add(new Message(JobSchedule.allMinutesPropertyName, "Must select at least one minute."));
			}
		}
		if (SELECTED_CODE.equals(bean.getAllHours())) {
			boolean found = false;
			for (int i = 0, l = 24; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "hour" + i))) {
					found = true;
					break;
				}
			}
			if (! found) {
				e.getMessages().add(new Message(JobSchedule.allHoursPropertyName, "Must select at least one hour."));
			}
		}
		if (SELECTED_CODE.equals(bean.getAllDays())) {
			boolean found = false;
			for (int i = 1, l = 32; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "day" + i))) {
					found = true;
					break;
				}
			}
			if (! found) {
				e.getMessages().add(new Message(JobSchedule.allDaysPropertyName, "Must select at least one day."));
			}
		}
		if (SELECTED_CODE.equals(bean.getAllMonths())) {
			boolean found = false;
			for (int i = 1, l = 13; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "month" + i))) {
					found = true;
					break;
				}
			}
			if (! found) {
				e.getMessages().add(new Message(JobSchedule.allMonthsPropertyName, "Must select at least one month."));
			}
		}
		if (SELECTED_CODE.equals(bean.getAllWeekdays())) {
			boolean found = false;
			for (int i = 1, l = 8; i < l; i++) {
				if (Boolean.TRUE.equals(Binder.get(bean, "weekday" + i))) {
					found = true;
					break;
				}
			}
			if (! found) {
				e.getMessages().add(new Message(JobSchedule.allWeekdaysPropertyName, "Must select at least one week day."));
			}
		}
	}
}
