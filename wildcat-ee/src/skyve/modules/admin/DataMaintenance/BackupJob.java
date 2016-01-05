package modules.admin.DataMaintenance;

import java.util.Date;
import java.util.List;

import org.skyve.CORE;
import org.skyve.job.WildcatJob;
import org.skyve.wildcat.backup.Backup;

public class BackupJob extends WildcatJob {
	private static final long serialVersionUID = 5924130498320912107L;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		Backup.backup();
		List<String> log = getLog();

		setPercentComplete(100);
		log.add("Finished Backup of customer " + CORE.getUser().getCustomerName() + " at " + new Date());
	}
}
