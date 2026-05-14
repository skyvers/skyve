package modules.admin.MailLog;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;

import modules.admin.domain.MailLog;

class MailLogBizletTest {

	@Test
	void testResolveReturnsArchivedMailLogWhenPresent() throws Exception {
		MailLog archived = new MailLog();
		MailLogService service = mock(MailLogService.class);
		when(service.retrieveFromArchives("mail-1")).thenReturn(archived);

		MailLogBizlet bizlet = new MailLogBizlet();
		injectService(bizlet, service);

		assertThat(bizlet.resolve("mail-1", null, null), is(archived));
	}

	@Test
	void testResolveReturnsNullWhenArchiveMisses() throws Exception {
		MailLogService service = mock(MailLogService.class);
		when(service.retrieveFromArchives("missing")).thenReturn(null);

		MailLogBizlet bizlet = new MailLogBizlet();
		injectService(bizlet, service);

		assertThat(bizlet.resolve("missing", null, null), is((MailLog) null));
	}

	private static void injectService(MailLogBizlet bizlet, MailLogService service) throws Exception {
		Field field = MailLogBizlet.class.getDeclaredField("mailLogService");
		field.setAccessible(true);
		field.set(bizlet, service);
	}
}
