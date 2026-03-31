package modules.admin.MailLog;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

class AdminModuleMailLogWiringTest {

	@Test
	void testAdminModuleContainsMailLogArchiveListWiring() throws Exception {
		String xml = Files.readString(Path.of("src/main/java/modules/admin/admin.xml"), StandardCharsets.UTF_8);

		assertThat(xml, containsString("<document ref=\"MailLog\" defaultQueryName=\"qMailLogs\""));
		assertThat(xml, containsString("<document ref=\"MailLogList\""));
		assertThat(xml, containsString("<edit document=\"MailLogList\" name=\"Mail Log\""));
		assertThat(xml, containsString("<query documentName=\"MailLog\" name=\"qMailLogs\""));
		assertThat(xml, containsString("<column binding=\"archiveTimestamp\" hidden=\"true\""));
		assertThat(xml, containsString("<column binding=\"archiveFilename\" hidden=\"true\""));
		assertThat(xml, containsString("<document name=\"MailLog\" permission=\"_R__C\""));
		assertThat(xml, containsString("<document name=\"MailLogList\" permission=\"_____\""));
	}
}
