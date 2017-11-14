package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.ReportDesign;

@SkyveFactory(testAction = false)
public class ReportDesignFactoryExtension extends ReportDesignFactory {

	@Override
	public ReportDesign getInstance() throws Exception {
		return super.getInstance();
	}

}
