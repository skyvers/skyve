package modules.test;

import java.io.File;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.backup.BackupJob;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.DataSensitivity;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class BackupTest extends AbstractSkyveTestDispose {
	private static File backupZip;
	private static AllAttributesPersistent aap;
	private static MappedExtensionJoinedStrategy mejs;
	private static MappedExtensionSingleStrategy mess;
	private static MappedSubclassedJoinedStrategy msjs;
	private static MappedSubclassedSingleStrategy msss;
	private static MappedExtensionSingleStrategyExtension messe;
	private static MappedExtensionJoinedStrategyExtension mejse;

	/**
	 * Ensure this test runs first by calling it from the restore tests.
	 */
	@Test
	public void testBackup() throws Exception {
		if (backupZip == null) {
			aap = Util.constructRandomInstance(u, m, aapd, 2);
			aap = p.save(aap);
			mejs = Util.constructRandomInstance(u, m, mejsd, 2);
			mejs = p.save(mejs);
			mess = Util.constructRandomInstance(u, m, messd, 2);
			mess = p.save(mess);
			msjs = Util.constructRandomInstance(u, m, msjsd, 2);
			msjs = p.save(msjs);
			msss = Util.constructRandomInstance(u, m, msssd, 2);
			msss = p.save(msss);
			messe = Util.constructRandomInstance(u, m, messd, 2);
			messe = p.save(messe);
			mejse = Util.constructRandomInstance(u, m, mejsd, 2);
			mejse = p.save(mejse);
			p.commit(false);
			p.begin();
			BackupJob job = new BackupJob();
			DataMaintenance bean = DataMaintenance.newInstance();
			bean.setDataSensitivity(DataSensitivity.internal);
			job.setBean(bean);
			job.execute();
			backupZip = job.getBackupZip();
		}
	}
	
	@Test
	public void testRestore1() throws Exception {
		testRestore(RestorePreProcess.deleteExistingTableDataUsingMetadata);
	}
	
	@Test
	public void testRestor2() throws Exception {
		testRestore(RestorePreProcess.dropTablesUsingBackupDropsqlRecreateTablesFromBackupCreatesql);
	}
	
	@Test
	public void testRestore3() throws Exception {
		testRestore(RestorePreProcess.dropTablesUsingBackupDropsqlRecreateTablesFromMetadata);
	}

	@Test
	public void testRestore4() throws Exception {
		testRestore(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromBackupCreatesql);
	}

	@Test
	public void testRestore5() throws Exception {
		testRestore(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromMetadata);
	}

	private void testRestore(RestorePreProcess restorePreProcess) throws Exception {
		if (backupZip == null) {
			testBackup();
		}

		// Restore the backup
		RestoreJob job = new RestoreJob();
		DataMaintenance bean = DataMaintenance.newInstance();
		String backupName = backupZip.getName();
		bean.setSelectedBackupName(backupName);
		bean.setContentRestoreOption(ContentRestoreOption.error);
		bean.setRestorePreProcess(restorePreProcess);
		job.setBean(bean);
		job.execute();
		
		// Compare the beans
		AllAttributesPersistent aapo = p.retrieve(aapd, aap.getBizId());
		compare(aap, aapo);
		MappedExtensionJoinedStrategy mejso = p.retrieve(mejsd, mejs.getBizId());
		compare(mejs, mejso);
		MappedExtensionSingleStrategy messo = p.retrieve(messd, mess.getBizId());
		compare(mess, messo);
		MappedSubclassedJoinedStrategy msjso = p.retrieve(msjsd, msjs.getBizId());
		compare(msjs, msjso);
		MappedSubclassedSingleStrategy mssso = p.retrieve(msssd, msss.getBizId());
		compare(msss, mssso);
		MappedExtensionSingleStrategyExtension messeo = p.retrieve(messd, messe.getBizId());
		compare(messe, messeo);
		MappedExtensionJoinedStrategyExtension mejseo = p.retrieve(mejsd, mejse.getBizId());
		compare(mejse, mejseo);
	}

	/**
	 * Compare all persistent attributes in the restored data to the original.
	 * @param one the original
	 * @param other	the restored
	 */
	@SuppressWarnings("null")
	private void compare(Bean one, Bean other) {
		if ((one == null) && (other == null)) {
			return;
		}
		Assert.assertEquals("bizModules", one.getBizModule(), other.getBizModule());
		Assert.assertEquals("bizDocuments", one.getBizDocument(), other.getBizDocument());
		
		Module module = c.getModule(one.getBizModule());
		Document d = module.getDocument(c, one.getBizDocument());
		for (Attribute a : d.getAllAttributes(c)) {
			if (a.isPersistent()) {
				String n = a.getName();
				if (a instanceof Field) {
					Assert.assertEquals(n, Binder.get(one, n), Binder.get(other, n));
				}
				else if (a instanceof Association) {
					compare((Bean) Binder.get(one, n), (Bean) Binder.get(other, n));
				}
				else if (a instanceof Collection) {
					@SuppressWarnings("unchecked")
					List<Bean> ones = (List<Bean>) Binder.get(one, n);
					@SuppressWarnings("unchecked")
					List<Bean> others = (List<Bean>) Binder.get(other, n);
					Assert.assertEquals(n + " sizes", ones.size(), others.size());
					for (int i = 0, l = ones.size(); i < l; i++) {
						compare(ones.get(i), others.get(i));
					}
				}
			}
		}
	}
	
	/**
	 * Delete the backup zip file
	 */
	@AfterClass
	public static void afterClass() {
		if (backupZip != null) {
			backupZip.delete();
		}
	}
}
