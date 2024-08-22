package modules.admin.ReportDesign.actions;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;

import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.generate.jasperreports.ReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.ReportDesignGeneratorFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.report.jasperreports.JasperReportUtil;
import org.skyve.impl.web.UserAgent;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.report.ReportFormat;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import modules.admin.ReportDesign.ReportDesignBizlet;
import modules.admin.domain.ReportDesign;

public class Preview extends DownloadAction<ReportDesign> {
	@Override
	public void prepare(ReportDesign bean, WebContext webContext)
	throws Exception {
		// Nothing to see here
	}

	@Override
	public Download download(ReportDesign bean, WebContext webContext) throws Exception {
		final DesignSpecification designSpecification = ReportDesignBizlet.specificationFromDesignBean(bean);
		final ReportDesignGenerator generator = ReportDesignGeneratorFactory.getGeneratorForDesign(designSpecification);

		generator.populateDesign(designSpecification);

		final User user = AbstractPersistence.get().getUser();
		final Customer customer = user.getCustomer();
		final Module module = customer.getModule(designSpecification.getModuleName());
		final Document document = module.getDocument(customer, designSpecification.getDocumentName());

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final String reportName = String.format("%s - %s.pdf", designSpecification.getModuleName(), designSpecification.getDocumentName());

		final JasperReportRenderer reportRenderer = new JasperReportRenderer(designSpecification);

		final Map<String, Object> parameters = new HashMap<>();

		HttpServletRequest request = EXT.getHttpServletRequest();
		final UxUi uxui = UserAgent.getUxUi(request);
		if (DesignSpecification.DefinitionSource.list.equals(designSpecification.getDefinitionSource())) {
			final String queryName = designSpecification.getQueryName();
			final String documentName = designSpecification.getDocumentName();
			final String documentOrQueryName = queryName != null ? queryName : documentName;
			// Note this checks user access and read permission
			final ListModel<Bean> listModel = JasperReportUtil.getQueryListModel(module, documentOrQueryName, uxui.getName());
			JasperReportUtil.runReport(reportRenderer.getReport(),
					user,
					parameters,
					// TODO: We could populate the list with random data.
					listModel,
					ReportFormat.pdf,
					baos);
		}
		else {
			final String moduleName = document.getOwningModuleName();
			final String documentName = document.getName();

			// Check access
			EXT.checkAccess(user, UserAccess.singular(moduleName, documentName), uxui.getName());

			// Check create
			if (! user.canCreateDocument(document)) {
				throw new SecurityException("create this data", user.getName());
			}
			parameters.put(JasperReportRenderer.DESIGN_SPEC_PARAMETER_NAME, designSpecification);
			JasperReportUtil.runReport(reportRenderer.getReport(),
					user,
					document,
					parameters,
					// TODO: We could populate the new instance with random data.
					document.newInstance(user),
					ReportFormat.pdf,
					baos);
		}

		return new Download(reportName, baos.toByteArray(), MimeType.pdf);
	}
}
