package modules.admin.ReportDesign.actions;

import org.skyve.impl.generate.jasperreports.Generator;
import org.skyve.impl.generate.jasperreports.Renderer;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ReportDesign.ReportDesignBizlet;
import modules.admin.domain.ReportDesign;

public class GenerateDefault implements ServerSideAction<ReportDesign> {

	private static final long serialVersionUID = -8203773871581974793L;

	@Override
	public ServerSideActionResult<ReportDesign> execute(ReportDesign bean, WebContext webContext) throws Exception {

		Generator generator = new Generator(ReportDesignBizlet.specificationFromDesignBean(bean));
		generator.generateDefaultDesign();
		bean.setJrxml(generator.getDesign().getJrxml());
		Renderer.saveJrxml(generator.getDesign());

		return new ServerSideActionResult<>(bean);
	}

}