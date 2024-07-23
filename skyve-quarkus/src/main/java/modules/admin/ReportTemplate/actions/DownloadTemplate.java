package modules.admin.ReportTemplate.actions;

import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.domain.ReportTemplate;
import modules.admin.domain.ReportTemplate.ReportType;

/**
 * Download the current report template as csv, html or xml depending on the 
 * report type and template type.
 */
public class DownloadTemplate extends DownloadAction<ReportTemplate> {
	@Override
	public void prepare(ReportTemplate bean, WebContext webContext) throws Exception {
		// nothing to do here
	}

	@Override
	public Download download(ReportTemplate bean, WebContext webContext) throws Exception {
		// return XML for Jasper reports
		if (bean.getReportType() == ReportType.jasper) {
			return new Download(String.format("%s Template.xml", bean.getName()), bean.getTemplate(), MimeType.xml);
		}

		// return TXT for Freemarker CSV reports
		if (ReportTemplate.OutputFormat.CSV == bean.getOutputFormat()) {
			return new Download(String.format("%s Template.txt", bean.getName()), bean.getTemplate(), MimeType.plain);
		}
		
		// return HTML for Freemarker PDF reports
		return new Download(String.format("%s Template.html", bean.getName()), bean.getTemplate(), MimeType.html);
	}
}
