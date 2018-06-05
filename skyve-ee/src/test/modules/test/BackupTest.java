package modules.test;

import java.io.File;

import org.junit.AfterClass;
import org.junit.Test;
import org.skyve.impl.backup.Backup;
import org.skyve.impl.backup.Restore;
import org.skyve.impl.backup.Restore.ContentRestoreOption;
import org.skyve.util.Util;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class BackupTest extends AbstractSkyveTest {
	private static File backupFile;
	
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
		backupFile = Backup.backup();
	}
	
	@Test
	public void testRestore() throws Exception {
		if (backupFile == null) {
			testBackup();
		}
		else {
			Restore.restore(backupFile.getName(), false, ContentRestoreOption.error);
		}
	}
	
	@Test
	public void testRestoreAgain() throws Exception {
		if (backupFile == null) {
			testBackup();
		}
		else {
			Restore.restore(backupFile.getName(), false, ContentRestoreOption.error);
		}
	}
	
	@AfterClass
	public static void afterClass() {
		if (backupFile != null) {
			backupFile.delete();
		}
	}
}
