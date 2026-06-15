package modules.admin.ReportTemplate.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;

import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ExportReportSpecificationH2Test extends AbstractH2Test {
	@Test
	void prepareDoesNotMutateBean() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setName("Specification");

		new ExportReportSpecification().prepare(bean, null);

		assertThat(bean.getName(), is("Specification"));
	}

	@Test
	void downloadReturnsJsonSpecificationForReportTemplate() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setName("Specification");
		bean.setTemplate("template body");

		Download download = new ExportReportSpecification().download(bean, null);

		assertThat(download.getFileName(), is("Specification.json"));
		assertThat(download.getMimeType(), is(MimeType.json));
		assertThat(download.getBytes(), is(notNullValue()));
		assertThat(new String(download.getBytes(), StandardCharsets.UTF_8), containsString("Specification"));
	}
}
