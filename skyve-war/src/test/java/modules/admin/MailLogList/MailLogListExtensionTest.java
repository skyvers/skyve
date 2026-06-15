package modules.admin.MailLogList;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.ArchivedDocumentCacheConfig;
import org.skyve.impl.util.UtilImpl;

import modules.admin.MailLog.MailLogService;

@SuppressWarnings("static-method")
class MailLogListExtensionTest {
	private final UtilImpl.ArchiveConfig originalArchiveConfig = UtilImpl.ARCHIVE_CONFIG;

	@AfterEach
	void afterEach() {
		UtilImpl.ARCHIVE_CONFIG = originalArchiveConfig;
	}

	@Test
	void testShowArchivedTrueWhenMailLogArchivingConfigured() throws Exception {
		UtilImpl.ARCHIVE_CONFIG = new UtilImpl.ArchiveConfig(300,
												100,
												java.util.List.of(new UtilImpl.ArchiveConfig.ArchiveDocConfig("admin", "MailLog", "maillog-archive", 30)),
												ArchivedDocumentCacheConfig.DEFAULT,
												UtilImpl.ArchiveConfig.ArchiveSchedule.DEFUALT);

		MailLogListExtension bean = new MailLogListExtension();
		injectService(bean, new MailLogService());

		assertTrue(bean.isShowArchived());
	}

	@Test
	void testShowArchivedFalseWhenMailLogArchivingDisabled() throws Exception {
		UtilImpl.ARCHIVE_CONFIG = UtilImpl.ArchiveConfig.DISABLED;

		MailLogListExtension bean = new MailLogListExtension();
		injectService(bean, new MailLogService());

		assertFalse(bean.isShowArchived());
		assertTrue(bean.isShowNonArchived());
	}

	private static void injectService(MailLogListExtension bean, MailLogService service) throws Exception {
		Field field = MailLogListExtension.class.getDeclaredField("mailLogService");
		field.setAccessible(true);
		field.set(bean, service);
	}
}
