package modules.admin.MailLogList;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.ArchivedDocumentCacheConfig;
import org.skyve.impl.util.UtilImpl;

import modules.admin.MailLog.MailLogService;

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

		assertThat(bean.isShowArchived(), is(true));
	}

	@Test
	void testShowArchivedFalseWhenMailLogArchivingDisabled() throws Exception {
		UtilImpl.ARCHIVE_CONFIG = UtilImpl.ArchiveConfig.DISABLED;

		MailLogListExtension bean = new MailLogListExtension();
		injectService(bean, new MailLogService());

		assertThat(bean.isShowArchived(), is(false));
		assertThat(bean.isShowNonArchived(), is(true));
	}

	private static void injectService(MailLogListExtension bean, MailLogService service) throws Exception {
		Field field = MailLogListExtension.class.getDeclaredField("mailLogService");
		field.setAccessible(true);
		field.set(bean, service);
	}
}
