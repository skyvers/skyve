package modules.admin.JobSchedule;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;

import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinition;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.parser.CronParser;

import modules.admin.domain.JobSchedule;

/**
 * Extends {@link JobSchedule} with display and conversion helpers.
 */
public class JobScheduleExtension extends JobSchedule {
	private static final long serialVersionUID = 1881085154489046318L;
	private static final CronDefinition CRON_DEFINITION = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);

	/**
	 * Gets a human-readable description of the cron schedule.
	 * 
	 * @return a natural-language schedule description
	 */
	@Override
	public String getScheduleString() {
		return CronDescriptor.instance().describe(new CronParser(CRON_DEFINITION).parse(getCronExpression()));
	}

	/**
	 * Generates a bizKey for the job schedule.
	 * The bizKey is a combination of the module name and the localized display name of the job.
	 * 
	 * @return a string in the format {@code Module - Job Display Name}, or an empty string if resolution fails
	 */
	public String bizKey() {
		try {
			String jobName = getJobName();
			int dotIndex = jobName.indexOf('.');
			Customer customer = CORE.getUser().getCustomer();
			Module module = customer.getModule(jobName.substring(0, dotIndex));
			JobMetaData job = module.getJob(jobName.substring(dotIndex + 1));

			return module.getName() + " - " + job.getLocalisedDisplayName();
		} catch (@SuppressWarnings("unused") Exception e) {
			return "";
		}
	}
	
	/**
	 * Converts this JobScheduleExtension to a core Skyve JobSchedule object.
	 * <p>
	 * Maps the following properties:
	 * <ul>
	 *   <li>bizId → uuid</li>
	 *   <li>jobName → jobName</li>
	 *   <li>cronExpression → cronExpression</li>
	 *   <li>startTime → startTime</li>
	 *   <li>endTime → endTime</li>
	 * </ul>
	 * 
	 * @return a new {@link org.skyve.job.JobSchedule} populated with this schedule values
	 */
	org.skyve.job.JobSchedule toJobSchedule() {
		org.skyve.job.JobSchedule result = new org.skyve.job.JobSchedule();
		result.setUuid(getBizId());
		result.setJobName(getJobName());
		result.setCronExpression(getCronExpression());
		result.setStartTime(getStartTime());
		result.setEndTime(getEndTime());
		return result;
	}
}
