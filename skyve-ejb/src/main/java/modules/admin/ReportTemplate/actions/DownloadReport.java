package modules.admin.ReportTemplate.actions;

import java.io.File;
import java.io.StringWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import freemarker.template.Template;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;

public class DownloadReport extends DownloadAction<ReportTemplateExtension> {
	@Override
	public void prepare(ReportTemplateExtension bean, WebContext webContext) throws Exception {
		// validate any required parameters
		ValidationException e = new ValidationException();
		for (int i = 0; i < bean.getParameters().size(); i++) {
			ReportParameterExtension p = bean.getParameters().get(i);
			p.validate(e, i);
		}

		if (e.getMessages().size() > 0) {
			throw e;
		}

		Template template = EXT.getReporting().getFreemarkerTemplate(bean.getTemplateName());

		Map<String, Object> root = new HashMap<>();

		// put the parameters into the root
		root.put("reportParameters", bean.getParameters());

		// put all the datasets into the root
		for (ReportDatasetExtension dataset : bean.getDatasets()) {
			DatasetType type = dataset.getDatasetType();
			if (type != null) {
				switch (type) {
					case bizQL:
						List<Bean> results = dataset.executeQuery();
						if (results != null) {
							root.put(dataset.getDatasetName(), results);
						}
						break;
					case SQL:
						List<DynaBean> sqlResults = dataset.executeSQLQuery();
						if (sqlResults != null) {
							root.put(dataset.getDatasetName(), sqlResults);
						}
						break;
					case constant:
						root.put(dataset.getDatasetName(), dataset.getQuery());
						break;
					case classValue:
						List<DynaBean> beanResults = dataset.executeClass();
						if (beanResults != null) {
							root.put(dataset.getDatasetName(), beanResults);
						}
						break;
					default:
						throw new IllegalStateException(type + " is not catered for");
				}
			}
		}

		try (StringWriter sw = new StringWriter()) {
			template.process(root, sw);
			bean.setResults(sw.toString());
		}
	}

	@Override
	public Download download(ReportTemplateExtension bean, WebContext webContext) throws Exception {
		// return the correct report type
		if (ReportTemplate.OutputFormat.CSV == bean.getOutputFormat()) {
			return new Download(String.format("%s.csv", bean.getName()), bean.getResults(), MimeType.csv);
		}

		// html report download
		// return new Download(String.format("%s.html", bean.getName()), inputStream, MimeType.html);

		// pdf report download
		Path tempDir = Paths.get(Util.getContentDirectory(), "temp");
		tempDir.toFile().mkdirs();

		File pdfFile = tempDir.resolve(String.format("%s.pdf", bean.getName())).toFile();
		pdfFile.deleteOnExit();
		
		EXT.getReporting().generateFreemarkerPDFFromHTML(bean.getResults(), pdfFile);
		
		return new Download(String.format("%s.pdf", bean.getName()), pdfFile, MimeType.pdf);
	}
}
