package modules.admin.MailLog;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.ArchivedDocumentCacheConfig;
import org.skyve.impl.archive.support.ArchiveRetriever;
import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.MailLog;

class MailLogServiceTest {
	private final UtilImpl.ArchiveConfig originalArchiveConfig = UtilImpl.ARCHIVE_CONFIG;

	@AfterEach
	void afterEach() {
		UtilImpl.ARCHIVE_CONFIG = originalArchiveConfig;
	}

	@Test
	@SuppressWarnings("static-method")
	void testMailLogDocConfigEmptyWhenArchivingNotConfigured() {
		UtilImpl.ARCHIVE_CONFIG = UtilImpl.ArchiveConfig.DISABLED;
		MailLogService service = new MailLogService();

		assertFalse(service.mailLogDocConfig().isPresent());
	}

	@Test
	@SuppressWarnings("static-method")
	void testRetrieveFromArchivesReturnsNullWhenArchivingNotConfigured() {
		UtilImpl.ARCHIVE_CONFIG = UtilImpl.ArchiveConfig.DISABLED;
		MailLogService service = new MailLogService();

		assertThat(service.retrieveFromArchives("mail-1"), is((MailLog) null));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMailLogDocConfigPresentWhenConfigured() {
		UtilImpl.ARCHIVE_CONFIG = new UtilImpl.ArchiveConfig(300,
												100,
												List.of(new UtilImpl.ArchiveConfig.ArchiveDocConfig("admin", "MailLog", "maillog-archive", 30)),
												ArchivedDocumentCacheConfig.DEFAULT,
												UtilImpl.ArchiveConfig.ArchiveSchedule.DEFUALT);
		MailLogService service = new MailLogService();

		assertTrue(service.mailLogDocConfig().isPresent());
		assertThat(service.mailLogDocConfig().get().directory(), is("maillog-archive"));
		assertEquals(30, service.mailLogDocConfig().get().retainDeletedDocumentsDays());
	}

	@Test
	@SuppressWarnings("static-method")
	void testRetrieveFromArchivesReturnsArchivedMailLogWhenPresent() throws Exception {
		UtilImpl.ARCHIVE_CONFIG = new UtilImpl.ArchiveConfig(300,
												100,
												List.of(new UtilImpl.ArchiveConfig.ArchiveDocConfig("admin", "MailLog", "maillog-archive", 30)),
												ArchivedDocumentCacheConfig.DEFAULT,
												UtilImpl.ArchiveConfig.ArchiveSchedule.DEFUALT);

		MailLog archived = new MailLog();
		ArchiveRetriever retriever = mock(ArchiveRetriever.class);
		when(retriever.retrieveByBizId(any(), eq("mail-1"))).thenReturn(Optional.of(archived));

		MailLogService service = new MailLogService();
		injectRetriever(service, retriever);

		assertThat(service.retrieveFromArchives("mail-1"), is(archived));
	}

	private static void injectRetriever(MailLogService service, ArchiveRetriever retriever) throws Exception {
		Field field = MailLogService.class.getDeclaredField("retriever");
		field.setAccessible(true);
		field.set(service, retriever);
	}
}
