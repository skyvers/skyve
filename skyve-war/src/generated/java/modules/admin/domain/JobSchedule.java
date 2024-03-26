package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.JobSchedule.JobScheduleExtension;
import modules.admin.UserProxy.UserProxyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;

/**
 * Job Schedule Entry
 * 
 * @navhas n runAs 1 UserProxy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class JobSchedule extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "JobSchedule";

	/** @hidden */
	public static final String jobNamePropertyName = "jobName";

	/** @hidden */
	public static final String cronExpressionPropertyName = "cronExpression";

	/** @hidden */
	public static final String allMinutesPropertyName = "allMinutes";

	/** @hidden */
	public static final String minute0PropertyName = "minute0";

	/** @hidden */
	public static final String minute1PropertyName = "minute1";

	/** @hidden */
	public static final String minute2PropertyName = "minute2";

	/** @hidden */
	public static final String minute3PropertyName = "minute3";

	/** @hidden */
	public static final String minute4PropertyName = "minute4";

	/** @hidden */
	public static final String minute5PropertyName = "minute5";

	/** @hidden */
	public static final String minute6PropertyName = "minute6";

	/** @hidden */
	public static final String minute7PropertyName = "minute7";

	/** @hidden */
	public static final String minute8PropertyName = "minute8";

	/** @hidden */
	public static final String minute9PropertyName = "minute9";

	/** @hidden */
	public static final String minute10PropertyName = "minute10";

	/** @hidden */
	public static final String minute11PropertyName = "minute11";

	/** @hidden */
	public static final String minute12PropertyName = "minute12";

	/** @hidden */
	public static final String minute13PropertyName = "minute13";

	/** @hidden */
	public static final String minute14PropertyName = "minute14";

	/** @hidden */
	public static final String minute15PropertyName = "minute15";

	/** @hidden */
	public static final String minute16PropertyName = "minute16";

	/** @hidden */
	public static final String minute17PropertyName = "minute17";

	/** @hidden */
	public static final String minute18PropertyName = "minute18";

	/** @hidden */
	public static final String minute19PropertyName = "minute19";

	/** @hidden */
	public static final String minute20PropertyName = "minute20";

	/** @hidden */
	public static final String minute21PropertyName = "minute21";

	/** @hidden */
	public static final String minute22PropertyName = "minute22";

	/** @hidden */
	public static final String minute23PropertyName = "minute23";

	/** @hidden */
	public static final String minute24PropertyName = "minute24";

	/** @hidden */
	public static final String minute25PropertyName = "minute25";

	/** @hidden */
	public static final String minute26PropertyName = "minute26";

	/** @hidden */
	public static final String minute27PropertyName = "minute27";

	/** @hidden */
	public static final String minute28PropertyName = "minute28";

	/** @hidden */
	public static final String minute29PropertyName = "minute29";

	/** @hidden */
	public static final String minute30PropertyName = "minute30";

	/** @hidden */
	public static final String minute31PropertyName = "minute31";

	/** @hidden */
	public static final String minute32PropertyName = "minute32";

	/** @hidden */
	public static final String minute33PropertyName = "minute33";

	/** @hidden */
	public static final String minute34PropertyName = "minute34";

	/** @hidden */
	public static final String minute35PropertyName = "minute35";

	/** @hidden */
	public static final String minute36PropertyName = "minute36";

	/** @hidden */
	public static final String minute37PropertyName = "minute37";

	/** @hidden */
	public static final String minute38PropertyName = "minute38";

	/** @hidden */
	public static final String minute39PropertyName = "minute39";

	/** @hidden */
	public static final String minute40PropertyName = "minute40";

	/** @hidden */
	public static final String minute41PropertyName = "minute41";

	/** @hidden */
	public static final String minute42PropertyName = "minute42";

	/** @hidden */
	public static final String minute43PropertyName = "minute43";

	/** @hidden */
	public static final String minute44PropertyName = "minute44";

	/** @hidden */
	public static final String minute45PropertyName = "minute45";

	/** @hidden */
	public static final String minute46PropertyName = "minute46";

	/** @hidden */
	public static final String minute47PropertyName = "minute47";

	/** @hidden */
	public static final String minute48PropertyName = "minute48";

	/** @hidden */
	public static final String minute49PropertyName = "minute49";

	/** @hidden */
	public static final String minute50PropertyName = "minute50";

	/** @hidden */
	public static final String minute51PropertyName = "minute51";

	/** @hidden */
	public static final String minute52PropertyName = "minute52";

	/** @hidden */
	public static final String minute53PropertyName = "minute53";

	/** @hidden */
	public static final String minute54PropertyName = "minute54";

	/** @hidden */
	public static final String minute55PropertyName = "minute55";

	/** @hidden */
	public static final String minute56PropertyName = "minute56";

	/** @hidden */
	public static final String minute57PropertyName = "minute57";

	/** @hidden */
	public static final String minute58PropertyName = "minute58";

	/** @hidden */
	public static final String minute59PropertyName = "minute59";

	/** @hidden */
	public static final String allHoursPropertyName = "allHours";

	/** @hidden */
	public static final String hour0PropertyName = "hour0";

	/** @hidden */
	public static final String hour1PropertyName = "hour1";

	/** @hidden */
	public static final String hour2PropertyName = "hour2";

	/** @hidden */
	public static final String hour3PropertyName = "hour3";

	/** @hidden */
	public static final String hour4PropertyName = "hour4";

	/** @hidden */
	public static final String hour5PropertyName = "hour5";

	/** @hidden */
	public static final String hour6PropertyName = "hour6";

	/** @hidden */
	public static final String hour7PropertyName = "hour7";

	/** @hidden */
	public static final String hour8PropertyName = "hour8";

	/** @hidden */
	public static final String hour9PropertyName = "hour9";

	/** @hidden */
	public static final String hour10PropertyName = "hour10";

	/** @hidden */
	public static final String hour11PropertyName = "hour11";

	/** @hidden */
	public static final String hour12PropertyName = "hour12";

	/** @hidden */
	public static final String hour13PropertyName = "hour13";

	/** @hidden */
	public static final String hour14PropertyName = "hour14";

	/** @hidden */
	public static final String hour15PropertyName = "hour15";

	/** @hidden */
	public static final String hour16PropertyName = "hour16";

	/** @hidden */
	public static final String hour17PropertyName = "hour17";

	/** @hidden */
	public static final String hour18PropertyName = "hour18";

	/** @hidden */
	public static final String hour19PropertyName = "hour19";

	/** @hidden */
	public static final String hour20PropertyName = "hour20";

	/** @hidden */
	public static final String hour21PropertyName = "hour21";

	/** @hidden */
	public static final String hour22PropertyName = "hour22";

	/** @hidden */
	public static final String hour23PropertyName = "hour23";

	/** @hidden */
	public static final String allDaysPropertyName = "allDays";

	/** @hidden */
	public static final String day1PropertyName = "day1";

	/** @hidden */
	public static final String day2PropertyName = "day2";

	/** @hidden */
	public static final String day3PropertyName = "day3";

	/** @hidden */
	public static final String day4PropertyName = "day4";

	/** @hidden */
	public static final String day5PropertyName = "day5";

	/** @hidden */
	public static final String day6PropertyName = "day6";

	/** @hidden */
	public static final String day7PropertyName = "day7";

	/** @hidden */
	public static final String day8PropertyName = "day8";

	/** @hidden */
	public static final String day9PropertyName = "day9";

	/** @hidden */
	public static final String day10PropertyName = "day10";

	/** @hidden */
	public static final String day11PropertyName = "day11";

	/** @hidden */
	public static final String day12PropertyName = "day12";

	/** @hidden */
	public static final String day13PropertyName = "day13";

	/** @hidden */
	public static final String day14PropertyName = "day14";

	/** @hidden */
	public static final String day15PropertyName = "day15";

	/** @hidden */
	public static final String day16PropertyName = "day16";

	/** @hidden */
	public static final String day17PropertyName = "day17";

	/** @hidden */
	public static final String day18PropertyName = "day18";

	/** @hidden */
	public static final String day19PropertyName = "day19";

	/** @hidden */
	public static final String day20PropertyName = "day20";

	/** @hidden */
	public static final String day21PropertyName = "day21";

	/** @hidden */
	public static final String day22PropertyName = "day22";

	/** @hidden */
	public static final String day23PropertyName = "day23";

	/** @hidden */
	public static final String day24PropertyName = "day24";

	/** @hidden */
	public static final String day25PropertyName = "day25";

	/** @hidden */
	public static final String day26PropertyName = "day26";

	/** @hidden */
	public static final String day27PropertyName = "day27";

	/** @hidden */
	public static final String day28PropertyName = "day28";

	/** @hidden */
	public static final String day29PropertyName = "day29";

	/** @hidden */
	public static final String day30PropertyName = "day30";

	/** @hidden */
	public static final String day31PropertyName = "day31";

	/** @hidden */
	public static final String allMonthsPropertyName = "allMonths";

	/** @hidden */
	public static final String month1PropertyName = "month1";

	/** @hidden */
	public static final String month2PropertyName = "month2";

	/** @hidden */
	public static final String month3PropertyName = "month3";

	/** @hidden */
	public static final String month4PropertyName = "month4";

	/** @hidden */
	public static final String month5PropertyName = "month5";

	/** @hidden */
	public static final String month6PropertyName = "month6";

	/** @hidden */
	public static final String month7PropertyName = "month7";

	/** @hidden */
	public static final String month8PropertyName = "month8";

	/** @hidden */
	public static final String month9PropertyName = "month9";

	/** @hidden */
	public static final String month10PropertyName = "month10";

	/** @hidden */
	public static final String month11PropertyName = "month11";

	/** @hidden */
	public static final String month12PropertyName = "month12";

	/** @hidden */
	public static final String allWeekdaysPropertyName = "allWeekdays";

	/** @hidden */
	public static final String weekday1PropertyName = "weekday1";

	/** @hidden */
	public static final String weekday2PropertyName = "weekday2";

	/** @hidden */
	public static final String weekday3PropertyName = "weekday3";

	/** @hidden */
	public static final String weekday4PropertyName = "weekday4";

	/** @hidden */
	public static final String weekday5PropertyName = "weekday5";

	/** @hidden */
	public static final String weekday6PropertyName = "weekday6";

	/** @hidden */
	public static final String weekday7PropertyName = "weekday7";

	/** @hidden */
	public static final String startTimePropertyName = "startTime";

	/** @hidden */
	public static final String endTimePropertyName = "endTime";

	/** @hidden */
	public static final String runAsPropertyName = "runAs";

	/** @hidden */
	public static final String jobScheduledImmediatelyPropertyName = "jobScheduledImmediately";

	/** @hidden */
	public static final String disabledPropertyName = "disabled";

	/** @hidden */
	public static final String scheduleStringPropertyName = "scheduleString";

	/**
	 * Job To Run
	 **/
	private String jobName;

	/**
	 * admin.jobSchedule.cronExppression.displayName
	 **/
	private String cronExpression;

	/**
	 * All Minutes
	 **/
	private String allMinutes;

	/**
	 * 00
	 **/
	private Boolean minute0;

	/**
	 * 01
	 **/
	private Boolean minute1;

	/**
	 * 02
	 **/
	private Boolean minute2;

	/**
	 * 03
	 **/
	private Boolean minute3;

	/**
	 * 04
	 **/
	private Boolean minute4;

	/**
	 * 05
	 **/
	private Boolean minute5;

	/**
	 * 06
	 **/
	private Boolean minute6;

	/**
	 * 07
	 **/
	private Boolean minute7;

	/**
	 * 08
	 **/
	private Boolean minute8;

	/**
	 * 09
	 **/
	private Boolean minute9;

	/**
	 * 10
	 **/
	private Boolean minute10;

	/**
	 * 11
	 **/
	private Boolean minute11;

	/**
	 * 12
	 **/
	private Boolean minute12;

	/**
	 * 13
	 **/
	private Boolean minute13;

	/**
	 * 14
	 **/
	private Boolean minute14;

	/**
	 * 15
	 **/
	private Boolean minute15;

	/**
	 * 16
	 **/
	private Boolean minute16;

	/**
	 * 17
	 **/
	private Boolean minute17;

	/**
	 * 18
	 **/
	private Boolean minute18;

	/**
	 * 19
	 **/
	private Boolean minute19;

	/**
	 * 20
	 **/
	private Boolean minute20;

	/**
	 * 21
	 **/
	private Boolean minute21;

	/**
	 * 22
	 **/
	private Boolean minute22;

	/**
	 * 23
	 **/
	private Boolean minute23;

	/**
	 * 24
	 **/
	private Boolean minute24;

	/**
	 * 25
	 **/
	private Boolean minute25;

	/**
	 * 26
	 **/
	private Boolean minute26;

	/**
	 * 27
	 **/
	private Boolean minute27;

	/**
	 * 28
	 **/
	private Boolean minute28;

	/**
	 * 29
	 **/
	private Boolean minute29;

	/**
	 * 30
	 **/
	private Boolean minute30;

	/**
	 * 31
	 **/
	private Boolean minute31;

	/**
	 * 32
	 **/
	private Boolean minute32;

	/**
	 * 33
	 **/
	private Boolean minute33;

	/**
	 * 34
	 **/
	private Boolean minute34;

	/**
	 * 35
	 **/
	private Boolean minute35;

	/**
	 * 36
	 **/
	private Boolean minute36;

	/**
	 * 37
	 **/
	private Boolean minute37;

	/**
	 * 38
	 **/
	private Boolean minute38;

	/**
	 * 39
	 **/
	private Boolean minute39;

	/**
	 * 40
	 **/
	private Boolean minute40;

	/**
	 * 41
	 **/
	private Boolean minute41;

	/**
	 * 42
	 **/
	private Boolean minute42;

	/**
	 * 43
	 **/
	private Boolean minute43;

	/**
	 * 44
	 **/
	private Boolean minute44;

	/**
	 * 45
	 **/
	private Boolean minute45;

	/**
	 * 46
	 **/
	private Boolean minute46;

	/**
	 * 47
	 **/
	private Boolean minute47;

	/**
	 * 48
	 **/
	private Boolean minute48;

	/**
	 * 49
	 **/
	private Boolean minute49;

	/**
	 * 50
	 **/
	private Boolean minute50;

	/**
	 * 51
	 **/
	private Boolean minute51;

	/**
	 * 52
	 **/
	private Boolean minute52;

	/**
	 * 53
	 **/
	private Boolean minute53;

	/**
	 * 54
	 **/
	private Boolean minute54;

	/**
	 * 55
	 **/
	private Boolean minute55;

	/**
	 * 56
	 **/
	private Boolean minute56;

	/**
	 * 57
	 **/
	private Boolean minute57;

	/**
	 * 58
	 **/
	private Boolean minute58;

	/**
	 * 59
	 **/
	private Boolean minute59;

	/**
	 * All Hours
	 **/
	private String allHours;

	/**
	 * 00
	 **/
	private Boolean hour0;

	/**
	 * 01
	 **/
	private Boolean hour1;

	/**
	 * 02
	 **/
	private Boolean hour2;

	/**
	 * 03
	 **/
	private Boolean hour3;

	/**
	 * 04
	 **/
	private Boolean hour4;

	/**
	 * 05
	 **/
	private Boolean hour5;

	/**
	 * 06
	 **/
	private Boolean hour6;

	/**
	 * 07
	 **/
	private Boolean hour7;

	/**
	 * 08
	 **/
	private Boolean hour8;

	/**
	 * 09
	 **/
	private Boolean hour9;

	/**
	 * 10
	 **/
	private Boolean hour10;

	/**
	 * 11
	 **/
	private Boolean hour11;

	/**
	 * 12
	 **/
	private Boolean hour12;

	/**
	 * 13
	 **/
	private Boolean hour13;

	/**
	 * 14
	 **/
	private Boolean hour14;

	/**
	 * 15
	 **/
	private Boolean hour15;

	/**
	 * 16
	 **/
	private Boolean hour16;

	/**
	 * 17
	 **/
	private Boolean hour17;

	/**
	 * 18
	 **/
	private Boolean hour18;

	/**
	 * 19
	 **/
	private Boolean hour19;

	/**
	 * 20
	 **/
	private Boolean hour20;

	/**
	 * 21
	 **/
	private Boolean hour21;

	/**
	 * 22
	 **/
	private Boolean hour22;

	/**
	 * 23
	 **/
	private Boolean hour23;

	/**
	 * All Days
	 **/
	private String allDays;

	/**
	 * 01
	 **/
	private Boolean day1;

	/**
	 * 02
	 **/
	private Boolean day2;

	/**
	 * 03
	 **/
	private Boolean day3;

	/**
	 * 04
	 **/
	private Boolean day4;

	/**
	 * 05
	 **/
	private Boolean day5;

	/**
	 * 06
	 **/
	private Boolean day6;

	/**
	 * 07
	 **/
	private Boolean day7;

	/**
	 * 08
	 **/
	private Boolean day8;

	/**
	 * 09
	 **/
	private Boolean day9;

	/**
	 * 10
	 **/
	private Boolean day10;

	/**
	 * 11
	 **/
	private Boolean day11;

	/**
	 * 12
	 **/
	private Boolean day12;

	/**
	 * 13
	 **/
	private Boolean day13;

	/**
	 * 14
	 **/
	private Boolean day14;

	/**
	 * 15
	 **/
	private Boolean day15;

	/**
	 * 16
	 **/
	private Boolean day16;

	/**
	 * 17
	 **/
	private Boolean day17;

	/**
	 * 18
	 **/
	private Boolean day18;

	/**
	 * 19
	 **/
	private Boolean day19;

	/**
	 * 20
	 **/
	private Boolean day20;

	/**
	 * 21
	 **/
	private Boolean day21;

	/**
	 * 22
	 **/
	private Boolean day22;

	/**
	 * 23
	 **/
	private Boolean day23;

	/**
	 * 24
	 **/
	private Boolean day24;

	/**
	 * 25
	 **/
	private Boolean day25;

	/**
	 * 26
	 **/
	private Boolean day26;

	/**
	 * 27
	 **/
	private Boolean day27;

	/**
	 * 28
	 **/
	private Boolean day28;

	/**
	 * 29
	 **/
	private Boolean day29;

	/**
	 * 30
	 **/
	private Boolean day30;

	/**
	 * 31
	 **/
	private Boolean day31;

	/**
	 * All Months
	 **/
	private String allMonths;

	/**
	 * Jan
	 **/
	private Boolean month1;

	/**
	 * Feb
	 **/
	private Boolean month2;

	/**
	 * Mar
	 **/
	private Boolean month3;

	/**
	 * Apr
	 **/
	private Boolean month4;

	/**
	 * May
	 **/
	private Boolean month5;

	/**
	 * Jun
	 **/
	private Boolean month6;

	/**
	 * Jul
	 **/
	private Boolean month7;

	/**
	 * Aug
	 **/
	private Boolean month8;

	/**
	 * Sep
	 **/
	private Boolean month9;

	/**
	 * Oct
	 **/
	private Boolean month10;

	/**
	 * Nov
	 **/
	private Boolean month11;

	/**
	 * Dec
	 **/
	private Boolean month12;

	/**
	 * All Weekdays
	 **/
	private String allWeekdays;

	/**
	 * Sun
	 **/
	private Boolean weekday1;

	/**
	 * Mon
	 **/
	private Boolean weekday2;

	/**
	 * Tue
	 **/
	private Boolean weekday3;

	/**
	 * Wed
	 **/
	private Boolean weekday4;

	/**
	 * Thu
	 **/
	private Boolean weekday5;

	/**
	 * Fri
	 **/
	private Boolean weekday6;

	/**
	 * Sat
	 **/
	private Boolean weekday7;

	/**
	 * Start Time
	 * <br/>
	 * When to start triggering the job.  May be left blank
	 **/
	private DateTime startTime;

	/**
	 * End Time
	 * <br/>
	 * When to finish triggering the job.  May be left blank
	 **/
	private DateTime endTime;

	/**
	 * Run As
	 * <br/>
	 * The user to run the job
	 **/
	private UserProxyExtension runAs = null;

	/**
	 * Job Scheduled Immediately
	 * <br/>
	 * Whether or not the job was scheduled immediately
	 **/
	private Boolean jobScheduledImmediately = Boolean.valueOf(false);

	/**
	 * Disabled
	 * <br/>
	 * Whether or not this schedule is disabled.
	 **/
	private Boolean disabled = Boolean.valueOf(false);

	/**
	 * Schedule
	 **/
	private String scheduleString;

	@Override
	@XmlTransient
	public String getBizModule() {
		return JobSchedule.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return JobSchedule.DOCUMENT_NAME;
	}

	public static JobScheduleExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return modules.admin.JobSchedule.JobScheduleBizlet.getBizKey(this);
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof JobSchedule) && 
					this.getBizId().equals(((JobSchedule) o).getBizId()));
	}

	/**
	 * {@link #jobName} accessor.
	 * @return	The value.
	 **/
	public String getJobName() {
		return jobName;
	}

	/**
	 * {@link #jobName} mutator.
	 * @param jobName	The new value.
	 **/
	@XmlElement
	public void setJobName(String jobName) {
		preset(jobNamePropertyName, jobName);
		this.jobName = jobName;
	}

	/**
	 * {@link #cronExpression} accessor.
	 * @return	The value.
	 **/
	public String getCronExpression() {
		return cronExpression;
	}

	/**
	 * {@link #cronExpression} mutator.
	 * @param cronExpression	The new value.
	 **/
	@XmlElement
	public void setCronExpression(String cronExpression) {
		preset(cronExpressionPropertyName, cronExpression);
		this.cronExpression = cronExpression;
	}

	/**
	 * {@link #allMinutes} accessor.
	 * @return	The value.
	 **/
	public String getAllMinutes() {
		return allMinutes;
	}

	/**
	 * {@link #allMinutes} mutator.
	 * @param allMinutes	The new value.
	 **/
	@XmlElement
	public void setAllMinutes(String allMinutes) {
		preset(allMinutesPropertyName, allMinutes);
		this.allMinutes = allMinutes;
	}

	/**
	 * {@link #minute0} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute0() {
		return minute0;
	}

	/**
	 * {@link #minute0} mutator.
	 * @param minute0	The new value.
	 **/
	@XmlElement
	public void setMinute0(Boolean minute0) {
		preset(minute0PropertyName, minute0);
		this.minute0 = minute0;
	}

	/**
	 * {@link #minute1} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute1() {
		return minute1;
	}

	/**
	 * {@link #minute1} mutator.
	 * @param minute1	The new value.
	 **/
	@XmlElement
	public void setMinute1(Boolean minute1) {
		preset(minute1PropertyName, minute1);
		this.minute1 = minute1;
	}

	/**
	 * {@link #minute2} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute2() {
		return minute2;
	}

	/**
	 * {@link #minute2} mutator.
	 * @param minute2	The new value.
	 **/
	@XmlElement
	public void setMinute2(Boolean minute2) {
		preset(minute2PropertyName, minute2);
		this.minute2 = minute2;
	}

	/**
	 * {@link #minute3} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute3() {
		return minute3;
	}

	/**
	 * {@link #minute3} mutator.
	 * @param minute3	The new value.
	 **/
	@XmlElement
	public void setMinute3(Boolean minute3) {
		preset(minute3PropertyName, minute3);
		this.minute3 = minute3;
	}

	/**
	 * {@link #minute4} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute4() {
		return minute4;
	}

	/**
	 * {@link #minute4} mutator.
	 * @param minute4	The new value.
	 **/
	@XmlElement
	public void setMinute4(Boolean minute4) {
		preset(minute4PropertyName, minute4);
		this.minute4 = minute4;
	}

	/**
	 * {@link #minute5} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute5() {
		return minute5;
	}

	/**
	 * {@link #minute5} mutator.
	 * @param minute5	The new value.
	 **/
	@XmlElement
	public void setMinute5(Boolean minute5) {
		preset(minute5PropertyName, minute5);
		this.minute5 = minute5;
	}

	/**
	 * {@link #minute6} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute6() {
		return minute6;
	}

	/**
	 * {@link #minute6} mutator.
	 * @param minute6	The new value.
	 **/
	@XmlElement
	public void setMinute6(Boolean minute6) {
		preset(minute6PropertyName, minute6);
		this.minute6 = minute6;
	}

	/**
	 * {@link #minute7} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute7() {
		return minute7;
	}

	/**
	 * {@link #minute7} mutator.
	 * @param minute7	The new value.
	 **/
	@XmlElement
	public void setMinute7(Boolean minute7) {
		preset(minute7PropertyName, minute7);
		this.minute7 = minute7;
	}

	/**
	 * {@link #minute8} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute8() {
		return minute8;
	}

	/**
	 * {@link #minute8} mutator.
	 * @param minute8	The new value.
	 **/
	@XmlElement
	public void setMinute8(Boolean minute8) {
		preset(minute8PropertyName, minute8);
		this.minute8 = minute8;
	}

	/**
	 * {@link #minute9} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute9() {
		return minute9;
	}

	/**
	 * {@link #minute9} mutator.
	 * @param minute9	The new value.
	 **/
	@XmlElement
	public void setMinute9(Boolean minute9) {
		preset(minute9PropertyName, minute9);
		this.minute9 = minute9;
	}

	/**
	 * {@link #minute10} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute10() {
		return minute10;
	}

	/**
	 * {@link #minute10} mutator.
	 * @param minute10	The new value.
	 **/
	@XmlElement
	public void setMinute10(Boolean minute10) {
		preset(minute10PropertyName, minute10);
		this.minute10 = minute10;
	}

	/**
	 * {@link #minute11} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute11() {
		return minute11;
	}

	/**
	 * {@link #minute11} mutator.
	 * @param minute11	The new value.
	 **/
	@XmlElement
	public void setMinute11(Boolean minute11) {
		preset(minute11PropertyName, minute11);
		this.minute11 = minute11;
	}

	/**
	 * {@link #minute12} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute12() {
		return minute12;
	}

	/**
	 * {@link #minute12} mutator.
	 * @param minute12	The new value.
	 **/
	@XmlElement
	public void setMinute12(Boolean minute12) {
		preset(minute12PropertyName, minute12);
		this.minute12 = minute12;
	}

	/**
	 * {@link #minute13} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute13() {
		return minute13;
	}

	/**
	 * {@link #minute13} mutator.
	 * @param minute13	The new value.
	 **/
	@XmlElement
	public void setMinute13(Boolean minute13) {
		preset(minute13PropertyName, minute13);
		this.minute13 = minute13;
	}

	/**
	 * {@link #minute14} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute14() {
		return minute14;
	}

	/**
	 * {@link #minute14} mutator.
	 * @param minute14	The new value.
	 **/
	@XmlElement
	public void setMinute14(Boolean minute14) {
		preset(minute14PropertyName, minute14);
		this.minute14 = minute14;
	}

	/**
	 * {@link #minute15} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute15() {
		return minute15;
	}

	/**
	 * {@link #minute15} mutator.
	 * @param minute15	The new value.
	 **/
	@XmlElement
	public void setMinute15(Boolean minute15) {
		preset(minute15PropertyName, minute15);
		this.minute15 = minute15;
	}

	/**
	 * {@link #minute16} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute16() {
		return minute16;
	}

	/**
	 * {@link #minute16} mutator.
	 * @param minute16	The new value.
	 **/
	@XmlElement
	public void setMinute16(Boolean minute16) {
		preset(minute16PropertyName, minute16);
		this.minute16 = minute16;
	}

	/**
	 * {@link #minute17} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute17() {
		return minute17;
	}

	/**
	 * {@link #minute17} mutator.
	 * @param minute17	The new value.
	 **/
	@XmlElement
	public void setMinute17(Boolean minute17) {
		preset(minute17PropertyName, minute17);
		this.minute17 = minute17;
	}

	/**
	 * {@link #minute18} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute18() {
		return minute18;
	}

	/**
	 * {@link #minute18} mutator.
	 * @param minute18	The new value.
	 **/
	@XmlElement
	public void setMinute18(Boolean minute18) {
		preset(minute18PropertyName, minute18);
		this.minute18 = minute18;
	}

	/**
	 * {@link #minute19} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute19() {
		return minute19;
	}

	/**
	 * {@link #minute19} mutator.
	 * @param minute19	The new value.
	 **/
	@XmlElement
	public void setMinute19(Boolean minute19) {
		preset(minute19PropertyName, minute19);
		this.minute19 = minute19;
	}

	/**
	 * {@link #minute20} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute20() {
		return minute20;
	}

	/**
	 * {@link #minute20} mutator.
	 * @param minute20	The new value.
	 **/
	@XmlElement
	public void setMinute20(Boolean minute20) {
		preset(minute20PropertyName, minute20);
		this.minute20 = minute20;
	}

	/**
	 * {@link #minute21} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute21() {
		return minute21;
	}

	/**
	 * {@link #minute21} mutator.
	 * @param minute21	The new value.
	 **/
	@XmlElement
	public void setMinute21(Boolean minute21) {
		preset(minute21PropertyName, minute21);
		this.minute21 = minute21;
	}

	/**
	 * {@link #minute22} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute22() {
		return minute22;
	}

	/**
	 * {@link #minute22} mutator.
	 * @param minute22	The new value.
	 **/
	@XmlElement
	public void setMinute22(Boolean minute22) {
		preset(minute22PropertyName, minute22);
		this.minute22 = minute22;
	}

	/**
	 * {@link #minute23} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute23() {
		return minute23;
	}

	/**
	 * {@link #minute23} mutator.
	 * @param minute23	The new value.
	 **/
	@XmlElement
	public void setMinute23(Boolean minute23) {
		preset(minute23PropertyName, minute23);
		this.minute23 = minute23;
	}

	/**
	 * {@link #minute24} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute24() {
		return minute24;
	}

	/**
	 * {@link #minute24} mutator.
	 * @param minute24	The new value.
	 **/
	@XmlElement
	public void setMinute24(Boolean minute24) {
		preset(minute24PropertyName, minute24);
		this.minute24 = minute24;
	}

	/**
	 * {@link #minute25} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute25() {
		return minute25;
	}

	/**
	 * {@link #minute25} mutator.
	 * @param minute25	The new value.
	 **/
	@XmlElement
	public void setMinute25(Boolean minute25) {
		preset(minute25PropertyName, minute25);
		this.minute25 = minute25;
	}

	/**
	 * {@link #minute26} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute26() {
		return minute26;
	}

	/**
	 * {@link #minute26} mutator.
	 * @param minute26	The new value.
	 **/
	@XmlElement
	public void setMinute26(Boolean minute26) {
		preset(minute26PropertyName, minute26);
		this.minute26 = minute26;
	}

	/**
	 * {@link #minute27} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute27() {
		return minute27;
	}

	/**
	 * {@link #minute27} mutator.
	 * @param minute27	The new value.
	 **/
	@XmlElement
	public void setMinute27(Boolean minute27) {
		preset(minute27PropertyName, minute27);
		this.minute27 = minute27;
	}

	/**
	 * {@link #minute28} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute28() {
		return minute28;
	}

	/**
	 * {@link #minute28} mutator.
	 * @param minute28	The new value.
	 **/
	@XmlElement
	public void setMinute28(Boolean minute28) {
		preset(minute28PropertyName, minute28);
		this.minute28 = minute28;
	}

	/**
	 * {@link #minute29} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute29() {
		return minute29;
	}

	/**
	 * {@link #minute29} mutator.
	 * @param minute29	The new value.
	 **/
	@XmlElement
	public void setMinute29(Boolean minute29) {
		preset(minute29PropertyName, minute29);
		this.minute29 = minute29;
	}

	/**
	 * {@link #minute30} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute30() {
		return minute30;
	}

	/**
	 * {@link #minute30} mutator.
	 * @param minute30	The new value.
	 **/
	@XmlElement
	public void setMinute30(Boolean minute30) {
		preset(minute30PropertyName, minute30);
		this.minute30 = minute30;
	}

	/**
	 * {@link #minute31} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute31() {
		return minute31;
	}

	/**
	 * {@link #minute31} mutator.
	 * @param minute31	The new value.
	 **/
	@XmlElement
	public void setMinute31(Boolean minute31) {
		preset(minute31PropertyName, minute31);
		this.minute31 = minute31;
	}

	/**
	 * {@link #minute32} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute32() {
		return minute32;
	}

	/**
	 * {@link #minute32} mutator.
	 * @param minute32	The new value.
	 **/
	@XmlElement
	public void setMinute32(Boolean minute32) {
		preset(minute32PropertyName, minute32);
		this.minute32 = minute32;
	}

	/**
	 * {@link #minute33} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute33() {
		return minute33;
	}

	/**
	 * {@link #minute33} mutator.
	 * @param minute33	The new value.
	 **/
	@XmlElement
	public void setMinute33(Boolean minute33) {
		preset(minute33PropertyName, minute33);
		this.minute33 = minute33;
	}

	/**
	 * {@link #minute34} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute34() {
		return minute34;
	}

	/**
	 * {@link #minute34} mutator.
	 * @param minute34	The new value.
	 **/
	@XmlElement
	public void setMinute34(Boolean minute34) {
		preset(minute34PropertyName, minute34);
		this.minute34 = minute34;
	}

	/**
	 * {@link #minute35} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute35() {
		return minute35;
	}

	/**
	 * {@link #minute35} mutator.
	 * @param minute35	The new value.
	 **/
	@XmlElement
	public void setMinute35(Boolean minute35) {
		preset(minute35PropertyName, minute35);
		this.minute35 = minute35;
	}

	/**
	 * {@link #minute36} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute36() {
		return minute36;
	}

	/**
	 * {@link #minute36} mutator.
	 * @param minute36	The new value.
	 **/
	@XmlElement
	public void setMinute36(Boolean minute36) {
		preset(minute36PropertyName, minute36);
		this.minute36 = minute36;
	}

	/**
	 * {@link #minute37} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute37() {
		return minute37;
	}

	/**
	 * {@link #minute37} mutator.
	 * @param minute37	The new value.
	 **/
	@XmlElement
	public void setMinute37(Boolean minute37) {
		preset(minute37PropertyName, minute37);
		this.minute37 = minute37;
	}

	/**
	 * {@link #minute38} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute38() {
		return minute38;
	}

	/**
	 * {@link #minute38} mutator.
	 * @param minute38	The new value.
	 **/
	@XmlElement
	public void setMinute38(Boolean minute38) {
		preset(minute38PropertyName, minute38);
		this.minute38 = minute38;
	}

	/**
	 * {@link #minute39} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute39() {
		return minute39;
	}

	/**
	 * {@link #minute39} mutator.
	 * @param minute39	The new value.
	 **/
	@XmlElement
	public void setMinute39(Boolean minute39) {
		preset(minute39PropertyName, minute39);
		this.minute39 = minute39;
	}

	/**
	 * {@link #minute40} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute40() {
		return minute40;
	}

	/**
	 * {@link #minute40} mutator.
	 * @param minute40	The new value.
	 **/
	@XmlElement
	public void setMinute40(Boolean minute40) {
		preset(minute40PropertyName, minute40);
		this.minute40 = minute40;
	}

	/**
	 * {@link #minute41} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute41() {
		return minute41;
	}

	/**
	 * {@link #minute41} mutator.
	 * @param minute41	The new value.
	 **/
	@XmlElement
	public void setMinute41(Boolean minute41) {
		preset(minute41PropertyName, minute41);
		this.minute41 = minute41;
	}

	/**
	 * {@link #minute42} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute42() {
		return minute42;
	}

	/**
	 * {@link #minute42} mutator.
	 * @param minute42	The new value.
	 **/
	@XmlElement
	public void setMinute42(Boolean minute42) {
		preset(minute42PropertyName, minute42);
		this.minute42 = minute42;
	}

	/**
	 * {@link #minute43} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute43() {
		return minute43;
	}

	/**
	 * {@link #minute43} mutator.
	 * @param minute43	The new value.
	 **/
	@XmlElement
	public void setMinute43(Boolean minute43) {
		preset(minute43PropertyName, minute43);
		this.minute43 = minute43;
	}

	/**
	 * {@link #minute44} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute44() {
		return minute44;
	}

	/**
	 * {@link #minute44} mutator.
	 * @param minute44	The new value.
	 **/
	@XmlElement
	public void setMinute44(Boolean minute44) {
		preset(minute44PropertyName, minute44);
		this.minute44 = minute44;
	}

	/**
	 * {@link #minute45} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute45() {
		return minute45;
	}

	/**
	 * {@link #minute45} mutator.
	 * @param minute45	The new value.
	 **/
	@XmlElement
	public void setMinute45(Boolean minute45) {
		preset(minute45PropertyName, minute45);
		this.minute45 = minute45;
	}

	/**
	 * {@link #minute46} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute46() {
		return minute46;
	}

	/**
	 * {@link #minute46} mutator.
	 * @param minute46	The new value.
	 **/
	@XmlElement
	public void setMinute46(Boolean minute46) {
		preset(minute46PropertyName, minute46);
		this.minute46 = minute46;
	}

	/**
	 * {@link #minute47} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute47() {
		return minute47;
	}

	/**
	 * {@link #minute47} mutator.
	 * @param minute47	The new value.
	 **/
	@XmlElement
	public void setMinute47(Boolean minute47) {
		preset(minute47PropertyName, minute47);
		this.minute47 = minute47;
	}

	/**
	 * {@link #minute48} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute48() {
		return minute48;
	}

	/**
	 * {@link #minute48} mutator.
	 * @param minute48	The new value.
	 **/
	@XmlElement
	public void setMinute48(Boolean minute48) {
		preset(minute48PropertyName, minute48);
		this.minute48 = minute48;
	}

	/**
	 * {@link #minute49} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute49() {
		return minute49;
	}

	/**
	 * {@link #minute49} mutator.
	 * @param minute49	The new value.
	 **/
	@XmlElement
	public void setMinute49(Boolean minute49) {
		preset(minute49PropertyName, minute49);
		this.minute49 = minute49;
	}

	/**
	 * {@link #minute50} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute50() {
		return minute50;
	}

	/**
	 * {@link #minute50} mutator.
	 * @param minute50	The new value.
	 **/
	@XmlElement
	public void setMinute50(Boolean minute50) {
		preset(minute50PropertyName, minute50);
		this.minute50 = minute50;
	}

	/**
	 * {@link #minute51} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute51() {
		return minute51;
	}

	/**
	 * {@link #minute51} mutator.
	 * @param minute51	The new value.
	 **/
	@XmlElement
	public void setMinute51(Boolean minute51) {
		preset(minute51PropertyName, minute51);
		this.minute51 = minute51;
	}

	/**
	 * {@link #minute52} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute52() {
		return minute52;
	}

	/**
	 * {@link #minute52} mutator.
	 * @param minute52	The new value.
	 **/
	@XmlElement
	public void setMinute52(Boolean minute52) {
		preset(minute52PropertyName, minute52);
		this.minute52 = minute52;
	}

	/**
	 * {@link #minute53} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute53() {
		return minute53;
	}

	/**
	 * {@link #minute53} mutator.
	 * @param minute53	The new value.
	 **/
	@XmlElement
	public void setMinute53(Boolean minute53) {
		preset(minute53PropertyName, minute53);
		this.minute53 = minute53;
	}

	/**
	 * {@link #minute54} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute54() {
		return minute54;
	}

	/**
	 * {@link #minute54} mutator.
	 * @param minute54	The new value.
	 **/
	@XmlElement
	public void setMinute54(Boolean minute54) {
		preset(minute54PropertyName, minute54);
		this.minute54 = minute54;
	}

	/**
	 * {@link #minute55} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute55() {
		return minute55;
	}

	/**
	 * {@link #minute55} mutator.
	 * @param minute55	The new value.
	 **/
	@XmlElement
	public void setMinute55(Boolean minute55) {
		preset(minute55PropertyName, minute55);
		this.minute55 = minute55;
	}

	/**
	 * {@link #minute56} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute56() {
		return minute56;
	}

	/**
	 * {@link #minute56} mutator.
	 * @param minute56	The new value.
	 **/
	@XmlElement
	public void setMinute56(Boolean minute56) {
		preset(minute56PropertyName, minute56);
		this.minute56 = minute56;
	}

	/**
	 * {@link #minute57} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute57() {
		return minute57;
	}

	/**
	 * {@link #minute57} mutator.
	 * @param minute57	The new value.
	 **/
	@XmlElement
	public void setMinute57(Boolean minute57) {
		preset(minute57PropertyName, minute57);
		this.minute57 = minute57;
	}

	/**
	 * {@link #minute58} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute58() {
		return minute58;
	}

	/**
	 * {@link #minute58} mutator.
	 * @param minute58	The new value.
	 **/
	@XmlElement
	public void setMinute58(Boolean minute58) {
		preset(minute58PropertyName, minute58);
		this.minute58 = minute58;
	}

	/**
	 * {@link #minute59} accessor.
	 * @return	The value.
	 **/
	public Boolean getMinute59() {
		return minute59;
	}

	/**
	 * {@link #minute59} mutator.
	 * @param minute59	The new value.
	 **/
	@XmlElement
	public void setMinute59(Boolean minute59) {
		preset(minute59PropertyName, minute59);
		this.minute59 = minute59;
	}

	/**
	 * {@link #allHours} accessor.
	 * @return	The value.
	 **/
	public String getAllHours() {
		return allHours;
	}

	/**
	 * {@link #allHours} mutator.
	 * @param allHours	The new value.
	 **/
	@XmlElement
	public void setAllHours(String allHours) {
		preset(allHoursPropertyName, allHours);
		this.allHours = allHours;
	}

	/**
	 * {@link #hour0} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour0() {
		return hour0;
	}

	/**
	 * {@link #hour0} mutator.
	 * @param hour0	The new value.
	 **/
	@XmlElement
	public void setHour0(Boolean hour0) {
		preset(hour0PropertyName, hour0);
		this.hour0 = hour0;
	}

	/**
	 * {@link #hour1} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour1() {
		return hour1;
	}

	/**
	 * {@link #hour1} mutator.
	 * @param hour1	The new value.
	 **/
	@XmlElement
	public void setHour1(Boolean hour1) {
		preset(hour1PropertyName, hour1);
		this.hour1 = hour1;
	}

	/**
	 * {@link #hour2} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour2() {
		return hour2;
	}

	/**
	 * {@link #hour2} mutator.
	 * @param hour2	The new value.
	 **/
	@XmlElement
	public void setHour2(Boolean hour2) {
		preset(hour2PropertyName, hour2);
		this.hour2 = hour2;
	}

	/**
	 * {@link #hour3} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour3() {
		return hour3;
	}

	/**
	 * {@link #hour3} mutator.
	 * @param hour3	The new value.
	 **/
	@XmlElement
	public void setHour3(Boolean hour3) {
		preset(hour3PropertyName, hour3);
		this.hour3 = hour3;
	}

	/**
	 * {@link #hour4} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour4() {
		return hour4;
	}

	/**
	 * {@link #hour4} mutator.
	 * @param hour4	The new value.
	 **/
	@XmlElement
	public void setHour4(Boolean hour4) {
		preset(hour4PropertyName, hour4);
		this.hour4 = hour4;
	}

	/**
	 * {@link #hour5} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour5() {
		return hour5;
	}

	/**
	 * {@link #hour5} mutator.
	 * @param hour5	The new value.
	 **/
	@XmlElement
	public void setHour5(Boolean hour5) {
		preset(hour5PropertyName, hour5);
		this.hour5 = hour5;
	}

	/**
	 * {@link #hour6} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour6() {
		return hour6;
	}

	/**
	 * {@link #hour6} mutator.
	 * @param hour6	The new value.
	 **/
	@XmlElement
	public void setHour6(Boolean hour6) {
		preset(hour6PropertyName, hour6);
		this.hour6 = hour6;
	}

	/**
	 * {@link #hour7} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour7() {
		return hour7;
	}

	/**
	 * {@link #hour7} mutator.
	 * @param hour7	The new value.
	 **/
	@XmlElement
	public void setHour7(Boolean hour7) {
		preset(hour7PropertyName, hour7);
		this.hour7 = hour7;
	}

	/**
	 * {@link #hour8} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour8() {
		return hour8;
	}

	/**
	 * {@link #hour8} mutator.
	 * @param hour8	The new value.
	 **/
	@XmlElement
	public void setHour8(Boolean hour8) {
		preset(hour8PropertyName, hour8);
		this.hour8 = hour8;
	}

	/**
	 * {@link #hour9} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour9() {
		return hour9;
	}

	/**
	 * {@link #hour9} mutator.
	 * @param hour9	The new value.
	 **/
	@XmlElement
	public void setHour9(Boolean hour9) {
		preset(hour9PropertyName, hour9);
		this.hour9 = hour9;
	}

	/**
	 * {@link #hour10} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour10() {
		return hour10;
	}

	/**
	 * {@link #hour10} mutator.
	 * @param hour10	The new value.
	 **/
	@XmlElement
	public void setHour10(Boolean hour10) {
		preset(hour10PropertyName, hour10);
		this.hour10 = hour10;
	}

	/**
	 * {@link #hour11} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour11() {
		return hour11;
	}

	/**
	 * {@link #hour11} mutator.
	 * @param hour11	The new value.
	 **/
	@XmlElement
	public void setHour11(Boolean hour11) {
		preset(hour11PropertyName, hour11);
		this.hour11 = hour11;
	}

	/**
	 * {@link #hour12} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour12() {
		return hour12;
	}

	/**
	 * {@link #hour12} mutator.
	 * @param hour12	The new value.
	 **/
	@XmlElement
	public void setHour12(Boolean hour12) {
		preset(hour12PropertyName, hour12);
		this.hour12 = hour12;
	}

	/**
	 * {@link #hour13} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour13() {
		return hour13;
	}

	/**
	 * {@link #hour13} mutator.
	 * @param hour13	The new value.
	 **/
	@XmlElement
	public void setHour13(Boolean hour13) {
		preset(hour13PropertyName, hour13);
		this.hour13 = hour13;
	}

	/**
	 * {@link #hour14} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour14() {
		return hour14;
	}

	/**
	 * {@link #hour14} mutator.
	 * @param hour14	The new value.
	 **/
	@XmlElement
	public void setHour14(Boolean hour14) {
		preset(hour14PropertyName, hour14);
		this.hour14 = hour14;
	}

	/**
	 * {@link #hour15} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour15() {
		return hour15;
	}

	/**
	 * {@link #hour15} mutator.
	 * @param hour15	The new value.
	 **/
	@XmlElement
	public void setHour15(Boolean hour15) {
		preset(hour15PropertyName, hour15);
		this.hour15 = hour15;
	}

	/**
	 * {@link #hour16} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour16() {
		return hour16;
	}

	/**
	 * {@link #hour16} mutator.
	 * @param hour16	The new value.
	 **/
	@XmlElement
	public void setHour16(Boolean hour16) {
		preset(hour16PropertyName, hour16);
		this.hour16 = hour16;
	}

	/**
	 * {@link #hour17} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour17() {
		return hour17;
	}

	/**
	 * {@link #hour17} mutator.
	 * @param hour17	The new value.
	 **/
	@XmlElement
	public void setHour17(Boolean hour17) {
		preset(hour17PropertyName, hour17);
		this.hour17 = hour17;
	}

	/**
	 * {@link #hour18} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour18() {
		return hour18;
	}

	/**
	 * {@link #hour18} mutator.
	 * @param hour18	The new value.
	 **/
	@XmlElement
	public void setHour18(Boolean hour18) {
		preset(hour18PropertyName, hour18);
		this.hour18 = hour18;
	}

	/**
	 * {@link #hour19} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour19() {
		return hour19;
	}

	/**
	 * {@link #hour19} mutator.
	 * @param hour19	The new value.
	 **/
	@XmlElement
	public void setHour19(Boolean hour19) {
		preset(hour19PropertyName, hour19);
		this.hour19 = hour19;
	}

	/**
	 * {@link #hour20} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour20() {
		return hour20;
	}

	/**
	 * {@link #hour20} mutator.
	 * @param hour20	The new value.
	 **/
	@XmlElement
	public void setHour20(Boolean hour20) {
		preset(hour20PropertyName, hour20);
		this.hour20 = hour20;
	}

	/**
	 * {@link #hour21} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour21() {
		return hour21;
	}

	/**
	 * {@link #hour21} mutator.
	 * @param hour21	The new value.
	 **/
	@XmlElement
	public void setHour21(Boolean hour21) {
		preset(hour21PropertyName, hour21);
		this.hour21 = hour21;
	}

	/**
	 * {@link #hour22} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour22() {
		return hour22;
	}

	/**
	 * {@link #hour22} mutator.
	 * @param hour22	The new value.
	 **/
	@XmlElement
	public void setHour22(Boolean hour22) {
		preset(hour22PropertyName, hour22);
		this.hour22 = hour22;
	}

	/**
	 * {@link #hour23} accessor.
	 * @return	The value.
	 **/
	public Boolean getHour23() {
		return hour23;
	}

	/**
	 * {@link #hour23} mutator.
	 * @param hour23	The new value.
	 **/
	@XmlElement
	public void setHour23(Boolean hour23) {
		preset(hour23PropertyName, hour23);
		this.hour23 = hour23;
	}

	/**
	 * {@link #allDays} accessor.
	 * @return	The value.
	 **/
	public String getAllDays() {
		return allDays;
	}

	/**
	 * {@link #allDays} mutator.
	 * @param allDays	The new value.
	 **/
	@XmlElement
	public void setAllDays(String allDays) {
		preset(allDaysPropertyName, allDays);
		this.allDays = allDays;
	}

	/**
	 * {@link #day1} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay1() {
		return day1;
	}

	/**
	 * {@link #day1} mutator.
	 * @param day1	The new value.
	 **/
	@XmlElement
	public void setDay1(Boolean day1) {
		preset(day1PropertyName, day1);
		this.day1 = day1;
	}

	/**
	 * {@link #day2} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay2() {
		return day2;
	}

	/**
	 * {@link #day2} mutator.
	 * @param day2	The new value.
	 **/
	@XmlElement
	public void setDay2(Boolean day2) {
		preset(day2PropertyName, day2);
		this.day2 = day2;
	}

	/**
	 * {@link #day3} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay3() {
		return day3;
	}

	/**
	 * {@link #day3} mutator.
	 * @param day3	The new value.
	 **/
	@XmlElement
	public void setDay3(Boolean day3) {
		preset(day3PropertyName, day3);
		this.day3 = day3;
	}

	/**
	 * {@link #day4} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay4() {
		return day4;
	}

	/**
	 * {@link #day4} mutator.
	 * @param day4	The new value.
	 **/
	@XmlElement
	public void setDay4(Boolean day4) {
		preset(day4PropertyName, day4);
		this.day4 = day4;
	}

	/**
	 * {@link #day5} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay5() {
		return day5;
	}

	/**
	 * {@link #day5} mutator.
	 * @param day5	The new value.
	 **/
	@XmlElement
	public void setDay5(Boolean day5) {
		preset(day5PropertyName, day5);
		this.day5 = day5;
	}

	/**
	 * {@link #day6} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay6() {
		return day6;
	}

	/**
	 * {@link #day6} mutator.
	 * @param day6	The new value.
	 **/
	@XmlElement
	public void setDay6(Boolean day6) {
		preset(day6PropertyName, day6);
		this.day6 = day6;
	}

	/**
	 * {@link #day7} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay7() {
		return day7;
	}

	/**
	 * {@link #day7} mutator.
	 * @param day7	The new value.
	 **/
	@XmlElement
	public void setDay7(Boolean day7) {
		preset(day7PropertyName, day7);
		this.day7 = day7;
	}

	/**
	 * {@link #day8} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay8() {
		return day8;
	}

	/**
	 * {@link #day8} mutator.
	 * @param day8	The new value.
	 **/
	@XmlElement
	public void setDay8(Boolean day8) {
		preset(day8PropertyName, day8);
		this.day8 = day8;
	}

	/**
	 * {@link #day9} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay9() {
		return day9;
	}

	/**
	 * {@link #day9} mutator.
	 * @param day9	The new value.
	 **/
	@XmlElement
	public void setDay9(Boolean day9) {
		preset(day9PropertyName, day9);
		this.day9 = day9;
	}

	/**
	 * {@link #day10} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay10() {
		return day10;
	}

	/**
	 * {@link #day10} mutator.
	 * @param day10	The new value.
	 **/
	@XmlElement
	public void setDay10(Boolean day10) {
		preset(day10PropertyName, day10);
		this.day10 = day10;
	}

	/**
	 * {@link #day11} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay11() {
		return day11;
	}

	/**
	 * {@link #day11} mutator.
	 * @param day11	The new value.
	 **/
	@XmlElement
	public void setDay11(Boolean day11) {
		preset(day11PropertyName, day11);
		this.day11 = day11;
	}

	/**
	 * {@link #day12} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay12() {
		return day12;
	}

	/**
	 * {@link #day12} mutator.
	 * @param day12	The new value.
	 **/
	@XmlElement
	public void setDay12(Boolean day12) {
		preset(day12PropertyName, day12);
		this.day12 = day12;
	}

	/**
	 * {@link #day13} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay13() {
		return day13;
	}

	/**
	 * {@link #day13} mutator.
	 * @param day13	The new value.
	 **/
	@XmlElement
	public void setDay13(Boolean day13) {
		preset(day13PropertyName, day13);
		this.day13 = day13;
	}

	/**
	 * {@link #day14} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay14() {
		return day14;
	}

	/**
	 * {@link #day14} mutator.
	 * @param day14	The new value.
	 **/
	@XmlElement
	public void setDay14(Boolean day14) {
		preset(day14PropertyName, day14);
		this.day14 = day14;
	}

	/**
	 * {@link #day15} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay15() {
		return day15;
	}

	/**
	 * {@link #day15} mutator.
	 * @param day15	The new value.
	 **/
	@XmlElement
	public void setDay15(Boolean day15) {
		preset(day15PropertyName, day15);
		this.day15 = day15;
	}

	/**
	 * {@link #day16} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay16() {
		return day16;
	}

	/**
	 * {@link #day16} mutator.
	 * @param day16	The new value.
	 **/
	@XmlElement
	public void setDay16(Boolean day16) {
		preset(day16PropertyName, day16);
		this.day16 = day16;
	}

	/**
	 * {@link #day17} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay17() {
		return day17;
	}

	/**
	 * {@link #day17} mutator.
	 * @param day17	The new value.
	 **/
	@XmlElement
	public void setDay17(Boolean day17) {
		preset(day17PropertyName, day17);
		this.day17 = day17;
	}

	/**
	 * {@link #day18} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay18() {
		return day18;
	}

	/**
	 * {@link #day18} mutator.
	 * @param day18	The new value.
	 **/
	@XmlElement
	public void setDay18(Boolean day18) {
		preset(day18PropertyName, day18);
		this.day18 = day18;
	}

	/**
	 * {@link #day19} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay19() {
		return day19;
	}

	/**
	 * {@link #day19} mutator.
	 * @param day19	The new value.
	 **/
	@XmlElement
	public void setDay19(Boolean day19) {
		preset(day19PropertyName, day19);
		this.day19 = day19;
	}

	/**
	 * {@link #day20} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay20() {
		return day20;
	}

	/**
	 * {@link #day20} mutator.
	 * @param day20	The new value.
	 **/
	@XmlElement
	public void setDay20(Boolean day20) {
		preset(day20PropertyName, day20);
		this.day20 = day20;
	}

	/**
	 * {@link #day21} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay21() {
		return day21;
	}

	/**
	 * {@link #day21} mutator.
	 * @param day21	The new value.
	 **/
	@XmlElement
	public void setDay21(Boolean day21) {
		preset(day21PropertyName, day21);
		this.day21 = day21;
	}

	/**
	 * {@link #day22} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay22() {
		return day22;
	}

	/**
	 * {@link #day22} mutator.
	 * @param day22	The new value.
	 **/
	@XmlElement
	public void setDay22(Boolean day22) {
		preset(day22PropertyName, day22);
		this.day22 = day22;
	}

	/**
	 * {@link #day23} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay23() {
		return day23;
	}

	/**
	 * {@link #day23} mutator.
	 * @param day23	The new value.
	 **/
	@XmlElement
	public void setDay23(Boolean day23) {
		preset(day23PropertyName, day23);
		this.day23 = day23;
	}

	/**
	 * {@link #day24} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay24() {
		return day24;
	}

	/**
	 * {@link #day24} mutator.
	 * @param day24	The new value.
	 **/
	@XmlElement
	public void setDay24(Boolean day24) {
		preset(day24PropertyName, day24);
		this.day24 = day24;
	}

	/**
	 * {@link #day25} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay25() {
		return day25;
	}

	/**
	 * {@link #day25} mutator.
	 * @param day25	The new value.
	 **/
	@XmlElement
	public void setDay25(Boolean day25) {
		preset(day25PropertyName, day25);
		this.day25 = day25;
	}

	/**
	 * {@link #day26} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay26() {
		return day26;
	}

	/**
	 * {@link #day26} mutator.
	 * @param day26	The new value.
	 **/
	@XmlElement
	public void setDay26(Boolean day26) {
		preset(day26PropertyName, day26);
		this.day26 = day26;
	}

	/**
	 * {@link #day27} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay27() {
		return day27;
	}

	/**
	 * {@link #day27} mutator.
	 * @param day27	The new value.
	 **/
	@XmlElement
	public void setDay27(Boolean day27) {
		preset(day27PropertyName, day27);
		this.day27 = day27;
	}

	/**
	 * {@link #day28} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay28() {
		return day28;
	}

	/**
	 * {@link #day28} mutator.
	 * @param day28	The new value.
	 **/
	@XmlElement
	public void setDay28(Boolean day28) {
		preset(day28PropertyName, day28);
		this.day28 = day28;
	}

	/**
	 * {@link #day29} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay29() {
		return day29;
	}

	/**
	 * {@link #day29} mutator.
	 * @param day29	The new value.
	 **/
	@XmlElement
	public void setDay29(Boolean day29) {
		preset(day29PropertyName, day29);
		this.day29 = day29;
	}

	/**
	 * {@link #day30} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay30() {
		return day30;
	}

	/**
	 * {@link #day30} mutator.
	 * @param day30	The new value.
	 **/
	@XmlElement
	public void setDay30(Boolean day30) {
		preset(day30PropertyName, day30);
		this.day30 = day30;
	}

	/**
	 * {@link #day31} accessor.
	 * @return	The value.
	 **/
	public Boolean getDay31() {
		return day31;
	}

	/**
	 * {@link #day31} mutator.
	 * @param day31	The new value.
	 **/
	@XmlElement
	public void setDay31(Boolean day31) {
		preset(day31PropertyName, day31);
		this.day31 = day31;
	}

	/**
	 * {@link #allMonths} accessor.
	 * @return	The value.
	 **/
	public String getAllMonths() {
		return allMonths;
	}

	/**
	 * {@link #allMonths} mutator.
	 * @param allMonths	The new value.
	 **/
	@XmlElement
	public void setAllMonths(String allMonths) {
		preset(allMonthsPropertyName, allMonths);
		this.allMonths = allMonths;
	}

	/**
	 * {@link #month1} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth1() {
		return month1;
	}

	/**
	 * {@link #month1} mutator.
	 * @param month1	The new value.
	 **/
	@XmlElement
	public void setMonth1(Boolean month1) {
		preset(month1PropertyName, month1);
		this.month1 = month1;
	}

	/**
	 * {@link #month2} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth2() {
		return month2;
	}

	/**
	 * {@link #month2} mutator.
	 * @param month2	The new value.
	 **/
	@XmlElement
	public void setMonth2(Boolean month2) {
		preset(month2PropertyName, month2);
		this.month2 = month2;
	}

	/**
	 * {@link #month3} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth3() {
		return month3;
	}

	/**
	 * {@link #month3} mutator.
	 * @param month3	The new value.
	 **/
	@XmlElement
	public void setMonth3(Boolean month3) {
		preset(month3PropertyName, month3);
		this.month3 = month3;
	}

	/**
	 * {@link #month4} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth4() {
		return month4;
	}

	/**
	 * {@link #month4} mutator.
	 * @param month4	The new value.
	 **/
	@XmlElement
	public void setMonth4(Boolean month4) {
		preset(month4PropertyName, month4);
		this.month4 = month4;
	}

	/**
	 * {@link #month5} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth5() {
		return month5;
	}

	/**
	 * {@link #month5} mutator.
	 * @param month5	The new value.
	 **/
	@XmlElement
	public void setMonth5(Boolean month5) {
		preset(month5PropertyName, month5);
		this.month5 = month5;
	}

	/**
	 * {@link #month6} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth6() {
		return month6;
	}

	/**
	 * {@link #month6} mutator.
	 * @param month6	The new value.
	 **/
	@XmlElement
	public void setMonth6(Boolean month6) {
		preset(month6PropertyName, month6);
		this.month6 = month6;
	}

	/**
	 * {@link #month7} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth7() {
		return month7;
	}

	/**
	 * {@link #month7} mutator.
	 * @param month7	The new value.
	 **/
	@XmlElement
	public void setMonth7(Boolean month7) {
		preset(month7PropertyName, month7);
		this.month7 = month7;
	}

	/**
	 * {@link #month8} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth8() {
		return month8;
	}

	/**
	 * {@link #month8} mutator.
	 * @param month8	The new value.
	 **/
	@XmlElement
	public void setMonth8(Boolean month8) {
		preset(month8PropertyName, month8);
		this.month8 = month8;
	}

	/**
	 * {@link #month9} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth9() {
		return month9;
	}

	/**
	 * {@link #month9} mutator.
	 * @param month9	The new value.
	 **/
	@XmlElement
	public void setMonth9(Boolean month9) {
		preset(month9PropertyName, month9);
		this.month9 = month9;
	}

	/**
	 * {@link #month10} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth10() {
		return month10;
	}

	/**
	 * {@link #month10} mutator.
	 * @param month10	The new value.
	 **/
	@XmlElement
	public void setMonth10(Boolean month10) {
		preset(month10PropertyName, month10);
		this.month10 = month10;
	}

	/**
	 * {@link #month11} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth11() {
		return month11;
	}

	/**
	 * {@link #month11} mutator.
	 * @param month11	The new value.
	 **/
	@XmlElement
	public void setMonth11(Boolean month11) {
		preset(month11PropertyName, month11);
		this.month11 = month11;
	}

	/**
	 * {@link #month12} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonth12() {
		return month12;
	}

	/**
	 * {@link #month12} mutator.
	 * @param month12	The new value.
	 **/
	@XmlElement
	public void setMonth12(Boolean month12) {
		preset(month12PropertyName, month12);
		this.month12 = month12;
	}

	/**
	 * {@link #allWeekdays} accessor.
	 * @return	The value.
	 **/
	public String getAllWeekdays() {
		return allWeekdays;
	}

	/**
	 * {@link #allWeekdays} mutator.
	 * @param allWeekdays	The new value.
	 **/
	@XmlElement
	public void setAllWeekdays(String allWeekdays) {
		preset(allWeekdaysPropertyName, allWeekdays);
		this.allWeekdays = allWeekdays;
	}

	/**
	 * {@link #weekday1} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday1() {
		return weekday1;
	}

	/**
	 * {@link #weekday1} mutator.
	 * @param weekday1	The new value.
	 **/
	@XmlElement
	public void setWeekday1(Boolean weekday1) {
		preset(weekday1PropertyName, weekday1);
		this.weekday1 = weekday1;
	}

	/**
	 * {@link #weekday2} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday2() {
		return weekday2;
	}

	/**
	 * {@link #weekday2} mutator.
	 * @param weekday2	The new value.
	 **/
	@XmlElement
	public void setWeekday2(Boolean weekday2) {
		preset(weekday2PropertyName, weekday2);
		this.weekday2 = weekday2;
	}

	/**
	 * {@link #weekday3} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday3() {
		return weekday3;
	}

	/**
	 * {@link #weekday3} mutator.
	 * @param weekday3	The new value.
	 **/
	@XmlElement
	public void setWeekday3(Boolean weekday3) {
		preset(weekday3PropertyName, weekday3);
		this.weekday3 = weekday3;
	}

	/**
	 * {@link #weekday4} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday4() {
		return weekday4;
	}

	/**
	 * {@link #weekday4} mutator.
	 * @param weekday4	The new value.
	 **/
	@XmlElement
	public void setWeekday4(Boolean weekday4) {
		preset(weekday4PropertyName, weekday4);
		this.weekday4 = weekday4;
	}

	/**
	 * {@link #weekday5} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday5() {
		return weekday5;
	}

	/**
	 * {@link #weekday5} mutator.
	 * @param weekday5	The new value.
	 **/
	@XmlElement
	public void setWeekday5(Boolean weekday5) {
		preset(weekday5PropertyName, weekday5);
		this.weekday5 = weekday5;
	}

	/**
	 * {@link #weekday6} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday6() {
		return weekday6;
	}

	/**
	 * {@link #weekday6} mutator.
	 * @param weekday6	The new value.
	 **/
	@XmlElement
	public void setWeekday6(Boolean weekday6) {
		preset(weekday6PropertyName, weekday6);
		this.weekday6 = weekday6;
	}

	/**
	 * {@link #weekday7} accessor.
	 * @return	The value.
	 **/
	public Boolean getWeekday7() {
		return weekday7;
	}

	/**
	 * {@link #weekday7} mutator.
	 * @param weekday7	The new value.
	 **/
	@XmlElement
	public void setWeekday7(Boolean weekday7) {
		preset(weekday7PropertyName, weekday7);
		this.weekday7 = weekday7;
	}

	/**
	 * {@link #startTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getStartTime() {
		return startTime;
	}

	/**
	 * {@link #startTime} mutator.
	 * @param startTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setStartTime(DateTime startTime) {
		preset(startTimePropertyName, startTime);
		this.startTime = startTime;
	}

	/**
	 * {@link #endTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getEndTime() {
		return endTime;
	}

	/**
	 * {@link #endTime} mutator.
	 * @param endTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setEndTime(DateTime endTime) {
		preset(endTimePropertyName, endTime);
		this.endTime = endTime;
	}

	/**
	 * {@link #runAs} accessor.
	 * @return	The value.
	 **/
	public UserProxyExtension getRunAs() {
		return runAs;
	}

	/**
	 * {@link #runAs} mutator.
	 * @param runAs	The new value.
	 **/
	@XmlElement
	public void setRunAs(UserProxyExtension runAs) {
		if (this.runAs != runAs) {
			preset(runAsPropertyName, runAs);
			this.runAs = runAs;
		}
	}

	/**
	 * {@link #jobScheduledImmediately} accessor.
	 * @return	The value.
	 **/
	public Boolean getJobScheduledImmediately() {
		return jobScheduledImmediately;
	}

	/**
	 * {@link #jobScheduledImmediately} mutator.
	 * @param jobScheduledImmediately	The new value.
	 **/
	@XmlElement
	public void setJobScheduledImmediately(Boolean jobScheduledImmediately) {
		preset(jobScheduledImmediatelyPropertyName, jobScheduledImmediately);
		this.jobScheduledImmediately = jobScheduledImmediately;
	}

	/**
	 * {@link #disabled} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisabled() {
		return disabled;
	}

	/**
	 * {@link #disabled} mutator.
	 * @param disabled	The new value.
	 **/
	@XmlElement
	public void setDisabled(Boolean disabled) {
		preset(disabledPropertyName, disabled);
		this.disabled = disabled;
	}

	/**
	 * {@link #scheduleString} accessor.
	 * @return	The value.
	 **/
	public String getScheduleString() {
		return scheduleString;
	}

	/**
	 * {@link #scheduleString} mutator.
	 * @param scheduleString	The new value.
	 **/
	@XmlElement
	public void setScheduleString(String scheduleString) {
		preset(scheduleStringPropertyName, scheduleString);
		this.scheduleString = scheduleString;
	}

	/**
	 * Whether this instance is configured to run schedule jobs
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isJobScheduler() {
		return (org.skyve.impl.util.UtilImpl.JOB_SCHEDULER);
	}

	/**
	 * {@link #isJobScheduler} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotJobScheduler() {
		return (! isJobScheduler());
	}

	/**
	 * scheduledImmediately
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isScheduledImmediately() {
		return (Boolean.TRUE.equals(getJobScheduledImmediately()));
	}

	/**
	 * {@link #isScheduledImmediately} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotScheduledImmediately() {
		return (! isScheduledImmediately());
	}

	/**
	 * True when Selected Days.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelectedDays() {
		return ("X".equals(getAllDays()));
	}

	/**
	 * {@link #isSelectedDays} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelectedDays() {
		return (! isSelectedDays());
	}

	/**
	 * True when Selected Hours.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelectedHours() {
		return ("X".equals(getAllHours()));
	}

	/**
	 * {@link #isSelectedHours} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelectedHours() {
		return (! isSelectedHours());
	}

	/**
	 * True when Selected Minutes.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelectedMinutes() {
		return ("X".equals(getAllMinutes()));
	}

	/**
	 * {@link #isSelectedMinutes} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelectedMinutes() {
		return (! isSelectedMinutes());
	}

	/**
	 * True when Selected Months.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelectedMonths() {
		return ("X".equals(getAllMonths()));
	}

	/**
	 * {@link #isSelectedMonths} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelectedMonths() {
		return (! isSelectedMonths());
	}

	/**
	 * True when Selected Weekdays.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelectedWeekdays() {
		return ("X".equals(getAllWeekdays()));
	}

	/**
	 * {@link #isSelectedWeekdays} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelectedWeekdays() {
		return (! isSelectedWeekdays());
	}
}
