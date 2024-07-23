package modules.admin.ReportTemplate.actions;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;
import org.skyve.web.WebContext;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportTemplate;
import modules.admin.domain.ReportTemplate.Mode;

/**
 * Used to test a Jasper {@link ReportTemplate}.
 */
public class TestJasperReport extends DownloadAction<ReportTemplate> {
	@Override
	public void prepare(ReportTemplate bean, WebContext webContext) throws Exception {
		// check there are no unsaved changes
		if (bean.isChanged()) {
			throw new DomainException("This report template has unsaved changes. Please save the template then try again.");
		}

		// check all test report parameters are provided
		ValidationException e = new ValidationException();
		for (ReportParameterExtension param : bean.getParameters()) {
			param.validateTest(e);
		}

		// check that an id parameter was provided if bean report
		if (bean.getMode() == Mode.bean) {
			boolean idParamProvided = false;
			for (ReportParameterExtension param : bean.getParameters()) {
				if (param.getName().equals(AbstractWebContext.ID_NAME)) {
					idParamProvided = true;
					break;
				}
			}

			if (!idParamProvided) {
				e.getMessages().add(
						new Message(
								"Must define parameter '" + AbstractWebContext.ID_NAME + "' to provide the bean for the report."));
			}
		}

		if (e.getMessages().size() > 0) {
			throw e;
		}
	}

	@Override
	public Download download(ReportTemplate bean, WebContext webContext) throws Exception {

		User user = CORE.getUser();
		Module module = user.getCustomer().getModule(bean.getModuleName());
		Document document = module.getDocument(user.getCustomer(), bean.getDocumentName());

		final Map<String, Object> jasperParams = new HashMap<>();

		// put the parameters into the root
		jasperParams.put("reportParameters", bean.getParameters());

		for (ReportParameterExtension param : bean.getParameters()) {
			switch (param.getType()) {
				case date:
					jasperParams.put(param.getName(), param.getDateTestValue());
					break;
				case integer:
					jasperParams.put(param.getName(), Integer.valueOf(param.getNumericalTestValue().intValue()));
					break;
				case longInteger:
					jasperParams.put(param.getName(), param.getNumericalTestValue());
					break;
				default:
					jasperParams.put(param.getName(), param.getTextTestValue());
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
