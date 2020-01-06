package modules.test;

import java.io.File;

import org.junit.AfterClass;
import org.junit.Test;
import org.skyve.impl.backup.BackupJob;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class BackupTest extends AbstractSkyveTestDispose {
	private static File backupZip;
	
	@Test
	public void testBackup() throws Exception {
		MappedExtensionJoinedStrategy mejs = Util.constructRandomInstance(u, m, mejsd, 3);
		p.save(mejs);
		MappedExtensionSingleStrategy mess = Util.constructRandomInstance(u, m, messd, 3);
		p.save(mess);
		MappedSubclassedJoinedStrategy msjs = Util.constructRandomInstance(u, m, msjsd, 3);
		p.save(msjs);
		MappedSubclassedSingleStrategy msss = Util.constructRandomInstance(u, m, msssd, 3);
		p.save(msss);
		MappedExtensionSingleStrategyExtension messe = Util.constructRandomInstance(u, m, messd, 3);
		p.save(messe);
		MappedExtensionJoinedStrategyExtension mejse = Util.constructRandomInstance(u, m, mejsd, 3);
		p.save(mejse);
		p.commit(false);
		BackupJob job = new BackupJob();
		job.setBean(new DataMaintenance());
		job.execute();
		backupZip = job.getBackupZip();
	}
	
	@Test
	public void testRestore() throws Exception {
		if (backupZip == null) {
			testBackup();
		}
		else {
			RestoreJob job = new RestoreJob();
			DataMaintenance bean = DataMaintenance.newInstance();
			String backupName = backupZip.getName();
			backupName = backupName.substring(0, backupName.length() - 4);
			bean.setSelectedBackupName(backupName);
			bean.setContentRestoreOption(ContentRestoreOption.error);
			job.setBean(bean);
			job.execute();
		}
	}
	
	@Test
	public void testRestoreAgain() throws Exception {
		testRestore();
	}
	
	@AfterClass
	public static void afterClass() {
		if (backupZip != null) {
			backupZip.delete();
		}
	}
}
