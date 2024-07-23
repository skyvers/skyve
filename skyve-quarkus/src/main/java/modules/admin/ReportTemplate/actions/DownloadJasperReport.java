package modules.admin.ReportTemplate.actions;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;
import org.skyve.web.WebContext;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;

/**
 * Used to download a Jasper {@link ReportTemplate}.
 */
public class DownloadJasperReport extends DownloadAction<ReportTemplateExtension> {
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
	}

	@Override
	public Download download(ReportTemplateExtension bean, WebContext webContext) throws Exception {

		User user = CORE.getUser();
		Module module = user.getCustomer().getModule(bean.getModuleName());
		Document document = module.getDocument(user.getCustomer(), bean.getDocumentName());

		Map<String, Object> jasperParams = new HashMap<>();

		// put the parameters into the root
		jasperParams.put("reportParameters", bean.getParameters());

		for (ReportParameterExtension param : bean.getParameters()) {
			switch (param.getType()) {
				case date:
					if (param.getReportInputValue() != null) {
						DateOnly date = CORE.getCustomer().getDefaultDateConverter().fromDisplayValue(param.getReportInputValue());
						jasperParams.put(param.getName(), date);
					} else {
						jasperParams.put(param.getName(), param.getDateDefaultValue());
					}
					break;
				case integer:
					if (param.getReportInputValue() != null) {
						jasperParams.put(param.getName(), Integer.getInteger(param.getReportInputValue()));
					} else {
						jasperParams.put(param.getName(), Integer.valueOf(param.getNumericalDefaultValue().intValue()));
					}
					break;
				case longInteger:
					if (param.getReportInputValue() != null) {
						jasperParams.put(param.getName(), Long.getLong(param.getReportInputValue()));
					} else {
						jasperParams.put(param.getName(), param.getNumericalDefaultValue());
					}
					break;
				default:
					if (param.getReportInputValue() != null) {
						jasperParams.put(param.getName(), param.getReportInputValue());
					} else {
						jasperParams.put(param.getName(), param.getTextDefaultValue());
					}
			}
		}

		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			// return the correct output format
			if (ReportTemplate.OutputFormat.CSV == bean.getOutputFormat()) {
				EXT.getReporting().runJasperBeanReport(user, document, bean.getReportName(), jasperParams, null, ReportFormat.csv, baos);
				return new Download(String.format("%s.csv", bean.getName()), baos.toByteArray(), MimeType.csv);
			}

			EXT.getReporting().runJasperBeanReport(user, document, bean.getReportName(), jasperParams, null, ReportFormat.pdf, baos);
			return new Download(String.format("%s.pdf", bean.getName()), baos.toByteArray(), MimeType.pdf);
		}
	}
}
