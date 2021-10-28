package modules.admin.JobSchedule;

import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinition;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.parser.CronParser;

import modules.admin.domain.JobSchedule;

public class JobScheduleExtension extends JobSchedule {

	private static final long serialVersionUID = 1881085154489046318L;
	private static final CronDefinition cronDefinition = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);
	
//	private final String ALL_CODE = "*";
//	private final String MULTIPLE_ENTRIES = ",";
//	private final Integer ALL_CODE_SPEC = Integer.valueOf(99);
//	private final String SELECTED_CODE = "X";
//	private final String LAST_DAY_CODE = "L";
//	private final String LAST_WEEK_DAY_CODE = "LW";
//	private final String ANY_CODE = "?";
//	private static final Integer ANY_CODE_SPEC = Integer.valueOf(98);
	
//	private final String SCHEDULE_STRING_START = "Runs ";
	
	@Override
	public String getScheduleString() {

		return CronDescriptor.instance().describe(new CronParser(cronDefinition).parse(getCronExpression()));
	}

//	public String getScheduleDescription() {
//		try {
//			return CronDescriptor.instance().describe(new CronParser(cronDefinition).parse(getCronExpression()));
//		} catch (@SuppressWarnings("unused") Exception e) {
//			return null;
//		}
//	}
	
//	public String cronToReadableString() throws ParseException {
//		
//		String result = "Runs at ";
////		.concat(SCHEDULE_STRING_START)
//		
////		Split CRON expression
//		JobCronExpression expression = new JobCronExpression(this.getCronExpression());
//		
//		Set<Integer> minutes = expression.getMinutes();
//		Set<Integer> hours = expression.getHours();
//		Set<Integer> days = expression.getDaysOfMonth();
//		Set<Integer> months = expression.getMonths();
//		Set<Integer> weekdays = expression.getDaysOfWeek();
//
//		
//// 		One hour and one minute selected
//		if (this.getAllMinutes() == null && this.getAllHours() == null
//			&& minutes.size() == 1 && hours.size() == 1) {
//			result.concat(hours.toString() + ":" + minutes.toString() + " ");
//		}
//		else {
//			result.concat(getMinutesString(minutes));
//			result.concat("past ").concat(getHoursString(hours));
//		}		
//		result.concat(" on");
//		
//		if (ALL_CODE.equals(getAllDays())) {
//			// deal with weekdays
//		}
//		else {
//			// deal with days of month
//		}
//		
//		result.concat(" in");
//		result.concat(getMonthsString(months));
//		
//		result.concat(".");
//		return result;
//	}
//	
//	public String getHoursString(Set<Integer> hours) {
//		Integer firstHour = hours.iterator().next();
//		String hoursString = "";
//		// Every hour
//		if (ALL_CODE.equals(getAllHours())) {
//			hoursString.concat(" every hour");
//		} 
//		// One selected hour
//		else if (hours.size() == 1) {
//			hoursString.concat(" hour " + hours.toString());
//		}
//		// Multiple selected hours
//		else {
//			for (Integer hour : hours) {
//				if (hour == firstHour) {
//					hoursString.concat(" hours " + hour.toString());
//				}
//				else if (hour != hours.size()-1) {
//					hoursString.concat(", " + hour.toString());
//				}
//				else {
//					hoursString.concat(" and " + hour.toString());
//				}
//			}
//		}
//		return hoursString;	
//	}
//	
//	public String getMinutesString(Set<Integer> minutes) {
//		Integer firstMinute = minutes.iterator().next();
//		String minutesString = "";
//		// Every minute
//		if (ALL_CODE.equals(getAllMinutes())) {
//			minutesString.concat(" every minute");
//		} 
//		// One selected minute
//		else if (minutes.size() == 1) {
//			minutesString.concat(" minute " + minutes.toString());
//		}
//		// Multiple selected minutes
//		else {
//			for (Integer minute : minutes) {
//				if (minute == firstMinute) {
//					minutesString.concat(minute.toString());
//				}
//				else if (minute != minutes.size()-1) {
//					minutesString.concat(", " + minute.toString());
//				}
//				else {
//					minutesString.concat(" and " + minute.toString());
//				}
//			}
//		}
//		return minutesString;
//	}
//	
//	public String getMonthsString(Set<Integer> months) {
//		Integer firstMonth = months.iterator().next();
//		String monthsString = "";
//		//Every month
//		if (ALL_CODE.equals(getAllMonths())) {
//			monthsString.concat(" each month");
//		}
//		// TODO get month string from int
//		// One selected month
//		else if (months.size() == 1) {
//			monthsString.concat(months.toString());
//		}
//		// Multiple selected months
//		else {
//			for (Integer month : months) {
//				if (month == firstMonth) {
//					monthsString.concat(month.toString());
//				}
//				else if (month != months.size()-1) {
//					monthsString.concat(", " + month.toString());
//				}
//				else {
//					monthsString.concat(" and " + month.toString());
//				}
//			}
//		}
//		return monthsString;
//	}
//	
//	public String monthsIntToString(Set<Integer> monthsList) {
//		String monthsString = "";
//		for (Integer month : monthsList) {
//			monthsString.concat("");
//		}
//		
//		return monthsString;
//	}
	
}
