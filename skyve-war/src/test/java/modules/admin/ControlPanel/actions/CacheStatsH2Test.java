package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ControlPanel.ControlPanelExtension;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class CacheStatsH2Test extends AbstractH2Test {
	@Test
	void executeStoresCacheStatsHtmlOnControlPanel() throws Exception {
		ControlPanelExtension bean = new ControlPanelExtension();

		ServerSideActionResult<ControlPanelExtension> result = new CacheStats().execute(bean, null);

		assertThat(result.getBean(), is(bean));
		assertThat(bean.getUnescapedResults(), is(notNullValue()));
		assertThat(bean.getUnescapedResults(), containsString("<table>"));
		assertThat(bean.getUnescapedResults(), containsString("</table>"));
		assertThat(bean.getTabIndex(), is(Integer.valueOf(2)));
	}
}
