package modules.test;

import org.junit.Test;
import org.skyve.impl.backup.Backup;
import org.skyve.util.Util;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class BackupTest extends AbstractSkyveTest {
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
		Backup.backup();
	}
}
