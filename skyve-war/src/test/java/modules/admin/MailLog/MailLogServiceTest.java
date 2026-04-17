package modules.admin.MailLog;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.ArchivedDocumentCacheConfig;
import org.skyve.impl.util.UtilImpl;

class MailLogServiceTest {
	private final UtilImpl.ArchiveConfig originalArchiveConfig = UtilImpl.ARCHIVE_CONFIG;

	@AfterEach
	void afterEach() {
		UtilImpl.ARCHIVE_CONFIG = originalArchiveConfig;
	}

	@Test
	void testMailLogDocConfigEmptyWhenArchivingNotConfigured() {
		UtilImpl.ARCHIVE_CONFIG = UtilImpl.ArchiveConfig.DISABLED;
		MailLogService service = new MailLogService();

		assertThat(service.mailLogDocConfig().isPresent(), is(false));
	}

	@Test
	void testMailLogDocConfigPresentWhenConfigured() {
		UtilImpl.ARCHIVE_CONFIG = new UtilImpl.ArchiveConfig(300,
												100,
												List.of(new UtilImpl.ArchiveConfig.ArchiveDocConfig("admin", "MailLog", "maillog-archive", 30)),
												ArchivedDocumentCacheConfig.DEFAULT,
												UtilImpl.ArchiveConfig.ArchiveSchedule.DEFUALT);
		MailLogService service = new MailLogService();

		assertThat(service.mailLogDocConfig().isPresent(), is(true));
		assertThat(service.mailLogDocConfig().get().directory(), is("maillog-archive"));
		assertThat(service.mailLogDocConfig().get().retainDeletedDocumentsDays(), is(30));
	}
}
