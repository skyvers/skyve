package modules.admin.ReportTemplate.actions;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.Charset;

import org.skyve.content.MimeType;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.domain.ReportTemplate;
import modules.admin.domain.ReportTemplate.ReportType;

/**
 * Download the current report template as csv, html or xml depending on the 
 * report type and template type.
 */
public class DownloadTemplate extends DownloadAction<ReportTemplate> {

	private static final long serialVersionUID = -723606290305446093L;

	@Override
	public void prepare(ReportTemplate bean, WebContext webContext) throws Exception {
	}

	@Override
	public Download download(ReportTemplate bean, WebContext webContext) throws Exception {

		// write the output string to an input stream
		InputStream inputStream = new ByteArrayInputStream(bean.getTemplate().getBytes(Charset.forName("UTF-8")));

		// return XML for Jasper reports
		if (bean.getReportType() == ReportType.jasper) {
			return new Download(String.format("%s Template.xml", bean.getName()), inputStream, MimeType.xml);
		}

		// return TXT for Freemarker CSV reports
		if (ReportTemplate.OutputFormat.CSV == bean.getOutputFormat()) {
			return new Download(String.format("%s Template.txt", bean.getName()), inputStream, MimeType.plain);
		}
		
		// return HTML for Freemarker PDF reports
		return new Download(String.format("%s Template.html", bean.getName()), inputStream, MimeType.html);
	}
}
