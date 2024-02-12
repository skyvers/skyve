package modules.admin.ReportDesign.actions;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.generate.jasperreports.Renderer;
import org.skyve.impl.generate.jasperreports.ReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.ReportDesignGeneratorFactory;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ReportDesign.ReportDesignBizlet;
import modules.admin.domain.ReportDesign;

public class GenerateDefault implements ServerSideAction<ReportDesign> {
	@Override
	public ServerSideActionResult<ReportDesign> execute(ReportDesign bean, WebContext webContext) throws Exception {
		final DesignSpecification designSpecification = ReportDesignBizlet.specificationFromDesignBean(bean);
		final ReportDesignGenerator generator = ReportDesignGeneratorFactory.getGeneratorForDesign(designSpecification);
		generator.populateDesign(designSpecification);

		final JasperReportRenderer reportRenderer = new JasperReportRenderer(designSpecification);

		bean.setJrxml(reportRenderer.getJrxml());
		if (bean.getRepositoryPath() != null) {
			Renderer.saveJrxml(designSpecification);
			webContext.growl(MessageSeverity.info, "Reports generated into " + bean.getRepositoryPath());
		}

		return new ServerSideActionResult<>(bean);
	}
}