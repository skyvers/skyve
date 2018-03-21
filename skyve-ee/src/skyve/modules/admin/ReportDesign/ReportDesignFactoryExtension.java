package modules.admin.ReportDesign;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.ReportDesign;
import modules.admin.util.ReportDesignFactory;

@SkyveFactory(testAction = false)
public class ReportDesignFactoryExtension extends ReportDesignFactory {

	@Override
	public ReportDesign getInstance() throws Exception {
		return super.getInstance();
	}

}
