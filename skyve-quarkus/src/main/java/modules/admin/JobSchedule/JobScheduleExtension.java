package modules.admin.JobSchedule;

import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinition;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.parser.CronParser;

import modules.admin.domain.JobSchedule;

public class JobScheduleExtension extends JobSchedule {
	private static final long serialVersionUID = 1881085154489046318L;

	private static final CronDefinition CRON_DEFINITION = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);
		
	@Override
	public String getScheduleString() {
		return CronDescriptor.instance().describe(new CronParser(CRON_DEFINITION).parse(getCronExpression()));
	}
}
