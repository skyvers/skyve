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

public class JobScheduleExtension extends JobSchedule {

	private static final long serialVersionUID = 1881085154489046318L;
	private static final CronDefinition CRON_DEFINITION = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);

	/**
	 * Gets a human-readable description of the cron schedule.
	 * 
	 * @return A string describing the schedule in natural language
	 */
	@Override
	public String getScheduleString() {
		return CronDescriptor.instance().describe(new CronParser(CRON_DEFINITION).parse(getCronExpression()));
	}

	/**
	 * Generates a bizKey for the job schedule.
	 * The bizKey is a combination of the module name and the localized display name of the job.
	 * 
	 * @return A string in the format "Module Name - Job Display Name", or an empty string if an error occurs.
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
}
