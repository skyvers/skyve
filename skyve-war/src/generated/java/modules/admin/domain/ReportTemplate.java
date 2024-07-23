package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.UserProxy.UserProxyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Report Template
 * <br/>
 * <p>The Report Template document is for report administrator users to create reports using a template.</p>
			<p>Report Template is persisted to the same table as a Report Configuration, so the template is always
			linked to the correct configuration record for the report.</p>
 * 
 * @depend - - - ReportType
 * @depend - - - OutputFormat
 * @depend - - - Mode
 * @depend - - - WizardState
 * @depend - - - GenerateExisting
 * @navhas n runAs 0..1 UserProxy
 * @navhas n usersToEmail 0..n UserProxy
 * @navcomposed 1 datasets 0..n ReportDataset
 * @navhas n editUsersToEmail 0..n Generic
 * @navcomposed 1 parameters 0..n ReportParameter
 * @navhas n newUserToEmail 0..1 UserProxy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class ReportTemplate extends AbstractPersistentBean implements org.skyve.domain.app.admin.ReportTemplate {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ReportTemplate";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String templateNamePropertyName = "templateName";

	/** @hidden */
	public static final String descriptionPropertyName = "description";

	/** @hidden */
	public static final String enabledPropertyName = "enabled";

	/** @hidden */
	public static final String includeFragmentPropertyName = "includeFragment";

	/** @hidden */
	public static final String reportTypePropertyName = "reportType";

	/** @hidden */
	public static final String outputFormatPropertyName = "outputFormat";

	/** @hidden */
	public static final String templatePropertyName = "template";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String reportNamePropertyName = "reportName";

	/** @hidden */
	public static final String modePropertyName = "mode";

	/** @hidden */
	public static final String restrictToRolePropertyName = "restrictToRole";

	/** @hidden */
	public static final String datasetsPropertyName = "datasets";

	/** @hidden */
	public static final String parametersPropertyName = "parameters";

	/** @hidden */
	public static final String scheduledPropertyName = "scheduled";

	/** @hidden */
	public static final String cronExpressionPropertyName = "cronExpression";

	/** @hidden */
	public static final String startTimePropertyName = "startTime";

	/** @hidden */
	public static final String endTimePropertyName = "endTime";

	/** @hidden */
	public static final String runAsPropertyName = "runAs";

	/** @hidden */
	public static final String usersToEmailPropertyName = "usersToEmail";

	/** @hidden */
	public static final String resultsPropertyName = "results";

	/** @hidden */
	public static final String wizardStatePropertyName = "wizardState";

	/** @hidden */
	public static final String generateExistingPropertyName = "generateExisting";

	/** @hidden */
	public static final String generateModuleNamePropertyName = "generateModuleName";

	/** @hidden */
	public static final String generateDocumentNamePropertyName = "generateDocumentName";

	/** @hidden */
	public static final String scheduleDescriptionPropertyName = "scheduleDescription";

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
	public static final String newUserToEmailPropertyName = "newUserToEmail";

	/** @hidden */
	public static final String editUsersToEmailPropertyName = "editUsersToEmail";

	/**
	 * Report Type
	 * <br/>
	 * Which template engine is being used to create this report?
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum ReportType implements Enumeration {
		jasper("Jasper", "Jasper"),
		freemarker("Freemarker", "Freemarker");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ReportType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ReportType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static ReportType fromCode(String code) {
			ReportType result = null;

			for (ReportType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ReportType fromLocalisedDescription(String description) {
			ReportType result = null;

			for (ReportType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Output Format
	 * <br/>
	 * What is the output format for this report?
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum OutputFormat implements Enumeration {
		CSV("CSV", "CSV"),
		PDF("PDF", "PDF");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(OutputFormat::toDomainValue).collect(Collectors.toUnmodifiableList());

		private OutputFormat(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static OutputFormat fromCode(String code) {
			OutputFormat result = null;

			for (OutputFormat value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static OutputFormat fromLocalisedDescription(String description) {
			OutputFormat result = null;

			for (OutputFormat value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Mode
	 * <br/>
	 * The query mode of the Jasper report
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Mode implements Enumeration {
		SQL("sql", "SQL"),
		bean("bean", "Bean");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Mode::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Mode(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static Mode fromCode(String code) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Mode fromLocalisedDescription(String description) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Wizard State
	 * <br/>
	 * The create template wizard is staged into the following states which roughly follow in order:
					<ul>
						<li>Enter basic details
						<li>Enter template markup
					</ul>
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum WizardState implements Enumeration {
		enterDetails("enterDetails", "enterDetails"),
		enterMarkup("enterMarkup", "enterMarkup");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(WizardState::toDomainValue).collect(Collectors.toUnmodifiableList());

		private WizardState(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static WizardState fromCode(String code) {
			WizardState result = null;

			for (WizardState value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static WizardState fromLocalisedDescription(String description) {
			WizardState result = null;

			for (WizardState value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Generate/Existing
	 * <br/>
	 * Do you want to generate a starting report template, or do you have existing 
				markup to enter directly?
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum GenerateExisting implements Enumeration {
		generate("Generate", "Generate"),
		existing("Existing", "Existing");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(GenerateExisting::toDomainValue).collect(Collectors.toUnmodifiableList());

		private GenerateExisting(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static GenerateExisting fromCode(String code) {
			GenerateExisting result = null;

			for (GenerateExisting value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static GenerateExisting fromLocalisedDescription(String description) {
			GenerateExisting result = null;

			for (GenerateExisting value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Name
	 * <br/>
	 * The name of this report
	 **/
	private String name;

	/**
	 * Template Name
	 * <br/>
	 * The internal name of the template, consists of the template name + the freemarker file 
				extension suffix so freemarker knows the encoding (should always be HTML).
	 **/
	private String templateName;

	/**
	 * Description
	 * <br/>
	 * Description of this report. Shown to users to help select which report to run.
	 **/
	private String description;

	/**
	 * Enabled
	 * <br/>
	 * Whether this report is enabled and able to be run as a report by users.
	 **/
	private Boolean enabled = Boolean.valueOf(true);

	/**
	 * Include Only/Subreport
	 * <br/>
	 * Whether this template is only intended to be included in other templates and is not a standalone report.
	 **/
	private Boolean includeFragment = Boolean.valueOf(false);

	/**
	 * Report Type
	 * <br/>
	 * Which template engine is being used to create this report?
	 **/
	private ReportType reportType = ReportType.freemarker;

	/**
	 * Output Format
	 * <br/>
	 * What is the output format for this report?
	 **/
	private OutputFormat outputFormat = OutputFormat.PDF;

	/**
	 * Template
	 * <br/>
	 * The report template markup.
	 * <br/>
	 * Not audited since template definition contains handlebars which inteferes with auditing.
	 **/
	private String template;

	/**
	 * Module Name
	 * <br/>
	 * The module where the report is located
	 **/
	private String moduleName;

	/**
	 * Document Name
	 * <br/>
	 * The document where the report is located
	 **/
	private String documentName;

	/**
	 * Report Name
	 * <br/>
	 * The name of the Jasper report
	 **/
	private String reportName;

	/**
	 * Mode
	 * <br/>
	 * The query mode of the Jasper report
	 **/
	private Mode mode;

	/**
	 * Restrict to Role
	 * <br/>
	 * If this report should only be available to a subset of users with a specific role
	 **/
	private String restrictToRole;

	/**
	 * Datasets
	 **/
	private List<ReportDatasetExtension> datasets = new ChangeTrackingArrayList<>("datasets", this);

	/**
	 * Parameters
	 **/
	private List<ReportParameterExtension> parameters = new ChangeTrackingArrayList<>("parameters", this);

	/**
	 * Scheduled
	 * <br/>
	 * Whether scheduling is enabled for this report or not.
	 * <br/>
	 * Whether or not this report is scheduled.
	 **/
	private Boolean scheduled = Boolean.valueOf(false);

	/**
	 * CRON Expression
	 **/
	private String cronExpression;

	/**
	 * Start Time
	 * <br/>
	 * When to start triggering the report.  May be left blank
	 **/
	private DateTime startTime;

	/**
	 * End Time
	 * <br/>
	 * When to finish triggering the report.  May be left blank
	 **/
	private DateTime endTime;

	/**
	 * Run As
	 * <br/>
	 * The user to run the report
	 **/
	private UserProxyExtension runAs = null;

	/**
	 * Users to Email
	 * <br/>
	 * The collection of users that will be recipients of this report.
	 **/
	private List<UserProxyExtension> usersToEmail = new ChangeTrackingArrayList<>("usersToEmail", this);

	/**
	 * Results
	 * <br/>
	 * Used as temporary storage for the preparation of report downloads.
	 **/
	private String results;

	/**
	 * Wizard State
	 * <br/>
	 * The create template wizard is staged into the following states which roughly follow in order:
					<ul>
						<li>Enter basic details
						<li>Enter template markup
					</ul>
	 **/
	private WizardState wizardState = WizardState.enterDetails;

	/**
	 * Generate/Existing
	 * <br/>
	 * Do you want to generate a starting report template, or do you have existing 
				markup to enter directly?
	 **/
	private GenerateExisting generateExisting;

	/**
	 * Module
	 * <br/>
	 * Module which contains the document to generate the report for
	 **/
	private String generateModuleName;

	/**
	 * Document
	 * <br/>
	 * Document to generate the report for
	 **/
	private String generateDocumentName;

	/**
	 * Schedule
	 **/
	private String scheduleDescription;

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
	 * All Months
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
	 * New Recipient
	 **/
	private UserProxyExtension newUserToEmail = null;

	/**
	 * Users to Email
	 * <br/>
	 * The collection of recipients shown in the view so users have permission to 
				remove. Modifies the underlying usersToEmail persistent collection.
	 **/
	private List<Generic> editUsersToEmail = new ChangeTrackingArrayList<>("editUsersToEmail", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return ReportTemplate.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ReportTemplate.DOCUMENT_NAME;
	}

	public static ReportTemplateExtension newInstance() {
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
return getName() != null ? String.format("Report - %s", getName()) : "New Report Template";
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ReportTemplate) && 
					this.getBizId().equals(((ReportTemplate) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #templateName} accessor.
	 * @return	The value.
	 **/
	public String getTemplateName() {
		return templateName;
	}

	/**
	 * {@link #templateName} mutator.
	 * @param templateName	The new value.
	 **/
	@XmlElement
	public void setTemplateName(String templateName) {
		preset(templateNamePropertyName, templateName);
		this.templateName = templateName;
	}

	/**
	 * {@link #description} accessor.
	 * @return	The value.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * @param description	The new value.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #enabled} accessor.
	 * @return	The value.
	 **/
	public Boolean getEnabled() {
		return enabled;
	}

	/**
	 * {@link #enabled} mutator.
	 * @param enabled	The new value.
	 **/
	@XmlElement
	public void setEnabled(Boolean enabled) {
		preset(enabledPropertyName, enabled);
		this.enabled = enabled;
	}

	/**
	 * {@link #includeFragment} accessor.
	 * @return	The value.
	 **/
	public Boolean getIncludeFragment() {
		return includeFragment;
	}

	/**
	 * {@link #includeFragment} mutator.
	 * @param includeFragment	The new value.
	 **/
	@XmlElement
	public void setIncludeFragment(Boolean includeFragment) {
		preset(includeFragmentPropertyName, includeFragment);
		this.includeFragment = includeFragment;
	}

	/**
	 * {@link #reportType} accessor.
	 * @return	The value.
	 **/
	public ReportType getReportType() {
		return reportType;
	}

	/**
	 * {@link #reportType} mutator.
	 * @param reportType	The new value.
	 **/
	@XmlElement
	public void setReportType(ReportType reportType) {
		preset(reportTypePropertyName, reportType);
		this.reportType = reportType;
	}

	/**
	 * {@link #outputFormat} accessor.
	 * @return	The value.
	 **/
	public OutputFormat getOutputFormat() {
		return outputFormat;
	}

	/**
	 * {@link #outputFormat} mutator.
	 * @param outputFormat	The new value.
	 **/
	@XmlElement
	public void setOutputFormat(OutputFormat outputFormat) {
		preset(outputFormatPropertyName, outputFormat);
		this.outputFormat = outputFormat;
	}

	/**
	 * {@link #template} accessor.
	 * @return	The value.
	 **/
	public String getTemplate() {
		return template;
	}

	/**
	 * {@link #template} mutator.
	 * @param template	The new value.
	 **/
	@XmlElement
	public void setTemplate(String template) {
		preset(templatePropertyName, template);
		this.template = template;
	}

	/**
	 * {@link #moduleName} accessor.
	 * @return	The value.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * @param moduleName	The new value.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 * @return	The value.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * @param documentName	The new value.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #reportName} accessor.
	 * @return	The value.
	 **/
	public String getReportName() {
		return reportName;
	}

	/**
	 * {@link #reportName} mutator.
	 * @param reportName	The new value.
	 **/
	@XmlElement
	public void setReportName(String reportName) {
		preset(reportNamePropertyName, reportName);
		this.reportName = reportName;
	}

	/**
	 * {@link #mode} accessor.
	 * @return	The value.
	 **/
	public Mode getMode() {
		return mode;
	}

	/**
	 * {@link #mode} mutator.
	 * @param mode	The new value.
	 **/
	@XmlElement
	public void setMode(Mode mode) {
		preset(modePropertyName, mode);
		this.mode = mode;
	}

	/**
	 * {@link #restrictToRole} accessor.
	 * @return	The value.
	 **/
	public String getRestrictToRole() {
		return restrictToRole;
	}

	/**
	 * {@link #restrictToRole} mutator.
	 * @param restrictToRole	The new value.
	 **/
	@XmlElement
	public void setRestrictToRole(String restrictToRole) {
		preset(restrictToRolePropertyName, restrictToRole);
		this.restrictToRole = restrictToRole;
	}

	/**
	 * {@link #datasets} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ReportDatasetExtension> getDatasets() {
		return datasets;
	}

	/**
	 * {@link #datasets} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ReportDatasetExtension getDatasetsElementById(String bizId) {
		return getElementById(datasets, bizId);
	}

	/**
	 * {@link #datasets} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDatasetsElementById(String bizId, ReportDatasetExtension element) {
		setElementById(datasets, element);
	}

	/**
	 * {@link #datasets} add.
	 * @param element	The element to add.
	 **/
	public boolean addDatasetsElement(ReportDatasetExtension element) {
		boolean result = datasets.add(element);
		if (result) {
			element.setParent((ReportTemplateExtension) this);
		}
		return result;
	}

	/**
	 * {@link #datasets} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDatasetsElement(int index, ReportDatasetExtension element) {
		datasets.add(index, element);
		element.setParent((ReportTemplateExtension) this);
	}

	/**
	 * {@link #datasets} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDatasetsElement(ReportDatasetExtension element) {
		boolean result = datasets.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #datasets} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ReportDatasetExtension removeDatasetsElement(int index) {
		ReportDatasetExtension result = datasets.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #parameters} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ReportParameterExtension> getParameters() {
		return parameters;
	}

	/**
	 * {@link #parameters} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ReportParameterExtension getParametersElementById(String bizId) {
		return getElementById(parameters, bizId);
	}

	/**
	 * {@link #parameters} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setParametersElementById(String bizId, ReportParameterExtension element) {
		setElementById(parameters, element);
	}

	/**
	 * {@link #parameters} add.
	 * @param element	The element to add.
	 **/
	public boolean addParametersElement(ReportParameterExtension element) {
		boolean result = parameters.add(element);
		if (result) {
			element.setParent((ReportTemplateExtension) this);
		}
		return result;
	}

	/**
	 * {@link #parameters} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addParametersElement(int index, ReportParameterExtension element) {
		parameters.add(index, element);
		element.setParent((ReportTemplateExtension) this);
	}

	/**
	 * {@link #parameters} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeParametersElement(ReportParameterExtension element) {
		boolean result = parameters.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #parameters} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ReportParameterExtension removeParametersElement(int index) {
		ReportParameterExtension result = parameters.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #scheduled} accessor.
	 * @return	The value.
	 **/
	public Boolean getScheduled() {
		return scheduled;
	}

	/**
	 * {@link #scheduled} mutator.
	 * @param scheduled	The new value.
	 **/
	@XmlElement
	public void setScheduled(Boolean scheduled) {
		preset(scheduledPropertyName, scheduled);
		this.scheduled = scheduled;
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
	 * {@link #usersToEmail} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<UserProxyExtension> getUsersToEmail() {
		return usersToEmail;
	}

	/**
	 * {@link #usersToEmail} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public UserProxyExtension getUsersToEmailElementById(String bizId) {
		return getElementById(usersToEmail, bizId);
	}

	/**
	 * {@link #usersToEmail} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setUsersToEmailElementById(String bizId, UserProxyExtension element) {
		setElementById(usersToEmail, element);
	}

	/**
	 * {@link #usersToEmail} add.
	 * @param element	The element to add.
	 **/
	public boolean addUsersToEmailElement(UserProxyExtension element) {
		return usersToEmail.add(element);
	}

	/**
	 * {@link #usersToEmail} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addUsersToEmailElement(int index, UserProxyExtension element) {
		usersToEmail.add(index, element);
	}

	/**
	 * {@link #usersToEmail} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeUsersToEmailElement(UserProxyExtension element) {
		return usersToEmail.remove(element);
	}

	/**
	 * {@link #usersToEmail} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public UserProxyExtension removeUsersToEmailElement(int index) {
		return usersToEmail.remove(index);
	}

	/**
	 * {@link #results} accessor.
	 * @return	The value.
	 **/
	public String getResults() {
		return results;
	}

	/**
	 * {@link #results} mutator.
	 * @param results	The new value.
	 **/
	@XmlElement
	public void setResults(String results) {
		this.results = results;
	}

	/**
	 * {@link #wizardState} accessor.
	 * @return	The value.
	 **/
	public WizardState getWizardState() {
		return wizardState;
	}

	/**
	 * {@link #wizardState} mutator.
	 * @param wizardState	The new value.
	 **/
	@XmlElement
	public void setWizardState(WizardState wizardState) {
		this.wizardState = wizardState;
	}

	/**
	 * {@link #generateExisting} accessor.
	 * @return	The value.
	 **/
	public GenerateExisting getGenerateExisting() {
		return generateExisting;
	}

	/**
	 * {@link #generateExisting} mutator.
	 * @param generateExisting	The new value.
	 **/
	@XmlElement
	public void setGenerateExisting(GenerateExisting generateExisting) {
		this.generateExisting = generateExisting;
	}

	/**
	 * {@link #generateModuleName} accessor.
	 * @return	The value.
	 **/
	public String getGenerateModuleName() {
		return generateModuleName;
	}

	/**
	 * {@link #generateModuleName} mutator.
	 * @param generateModuleName	The new value.
	 **/
	@XmlElement
	public void setGenerateModuleName(String generateModuleName) {
		this.generateModuleName = generateModuleName;
	}

	/**
	 * {@link #generateDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getGenerateDocumentName() {
		return generateDocumentName;
	}

	/**
	 * {@link #generateDocumentName} mutator.
	 * @param generateDocumentName	The new value.
	 **/
	@XmlElement
	public void setGenerateDocumentName(String generateDocumentName) {
		this.generateDocumentName = generateDocumentName;
	}

	/**
	 * {@link #scheduleDescription} accessor.
	 * @return	The value.
	 **/
	public String getScheduleDescription() {
		return scheduleDescription;
	}

	/**
	 * {@link #scheduleDescription} mutator.
	 * @param scheduleDescription	The new value.
	 **/
	@XmlElement
	public void setScheduleDescription(String scheduleDescription) {
		this.scheduleDescription = scheduleDescription;
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
	 * {@link #newUserToEmail} accessor.
	 * @return	The value.
	 **/
	public UserProxyExtension getNewUserToEmail() {
		return newUserToEmail;
	}

	/**
	 * {@link #newUserToEmail} mutator.
	 * @param newUserToEmail	The new value.
	 **/
	@XmlElement
	public void setNewUserToEmail(UserProxyExtension newUserToEmail) {
		if (this.newUserToEmail != newUserToEmail) {
			this.newUserToEmail = newUserToEmail;
		}
	}

	/**
	 * {@link #editUsersToEmail} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Generic> getEditUsersToEmail() {
		return editUsersToEmail;
	}

	/**
	 * {@link #editUsersToEmail} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Generic getEditUsersToEmailElementById(String bizId) {
		return getElementById(editUsersToEmail, bizId);
	}

	/**
	 * {@link #editUsersToEmail} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setEditUsersToEmailElementById(String bizId, Generic element) {
		setElementById(editUsersToEmail, element);
	}

	/**
	 * {@link #editUsersToEmail} add.
	 * @param element	The element to add.
	 **/
	public boolean addEditUsersToEmailElement(Generic element) {
		return editUsersToEmail.add(element);
	}

	/**
	 * {@link #editUsersToEmail} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addEditUsersToEmailElement(int index, Generic element) {
		editUsersToEmail.add(index, element);
	}

	/**
	 * {@link #editUsersToEmail} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeEditUsersToEmailElement(Generic element) {
		return editUsersToEmail.remove(element);
	}

	/**
	 * {@link #editUsersToEmail} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public Generic removeEditUsersToEmailElement(int index) {
		return editUsersToEmail.remove(index);
	}

	/**
	 * Shows the Scheduling tab if the user has permissions to save changes to reports.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCanSchedule() {
		return (isUserInOwningModuleRole("BasicUser") 
						|| isUserInOwningModuleRole("DevOps")
						|| isUserInOwningModuleRole("SecurityAdministrator"));
	}

	/**
	 * {@link #isCanSchedule} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCanSchedule() {
		return (! isCanSchedule());
	}

	/**
	 * True when this ReportTemplate has been created, used to determine when to show create vs edit view.
	 *
	 * @return The condition
	 */
	@XmlTransient
	@Override
	public boolean isCreated() {
		return (isPersisted());
	}

	/**
	 * {@link #isCreated} negation.
	 *
	 * @return The negated condition
	 */
	@Override
	public boolean isNotCreated() {
		return (! isCreated());
	}

	/**
	 * Enter Report Details step
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEnterDetails() {
		return (WizardState.enterDetails == getWizardState());
	}

	/**
	 * {@link #isEnterDetails} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEnterDetails() {
		return (! isEnterDetails());
	}

	/**
	 * True when the user has existing markup to enter
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEnterExisting() {
		return (getGenerateExisting() == GenerateExisting.existing);
	}

	/**
	 * {@link #isEnterExisting} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEnterExisting() {
		return (! isEnterExisting());
	}

	/**
	 * True when the user wants to generate new markup
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEnterGenerate() {
		return (getGenerateExisting() == GenerateExisting.generate);
	}

	/**
	 * {@link #isEnterGenerate} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEnterGenerate() {
		return (! isEnterGenerate());
	}

	/**
	 * Enter Report Markup step
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEnterMarkup() {
		return (WizardState.enterMarkup == getWizardState());
	}

	/**
	 * {@link #isEnterMarkup} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEnterMarkup() {
		return (! isEnterMarkup());
	}

	/**
	 * True if this report has parameters for the user to input. Used to show the parameter 
				grid for non-admin users when running a report.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isHasParameters() {
		return (getParameters().size() > 0);
	}

	/**
	 * {@link #isHasParameters} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotHasParameters() {
		return (! isHasParameters());
	}

	/**
	 * True when scheduling is enabled, shows the scheduling fields in the UI.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSchedulingEnabled() {
		return (Boolean.TRUE.equals(getScheduled()));
	}

	/**
	 * {@link #isSchedulingEnabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSchedulingEnabled() {
		return (! isSchedulingEnabled());
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

	/**
	 * Show Next Button
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowNext() {
		return (isEnterDetails());
	}

	/**
	 * {@link #isShowNext} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowNext() {
		return (! isShowNext());
	}

	/**
	 * True when this report is a Freemarker report
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTypeFreemarker() {
		return (ReportType.freemarker == getReportType());
	}

	/**
	 * {@link #isTypeFreemarker} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTypeFreemarker() {
		return (! isTypeFreemarker());
	}

	/**
	 * True when this report is a Jasper report
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTypeJasper() {
		return (ReportType.jasper == getReportType());
	}

	/**
	 * {@link #isTypeJasper} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTypeJasper() {
		return (! isTypeJasper());
	}

	/**
	 * Controls whether this report template should be accessible and visible to the 
                current user, specifically if the user has the DevOps role.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isValidAccess() {
		return (isUserInOwningModuleRole("DevOps"));
	}

	/**
	 * {@link #isValidAccess} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotValidAccess() {
		return (! isValidAccess());
	}
}
