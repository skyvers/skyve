package modules.kitchensink.FluentDefaultWidgetTest.actions;

import modules.kitchensink.domain.FluentDefaultWidgetTest;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class GenerateXMLTest extends AbstractActionTest<FluentDefaultWidgetTest, GenerateXML> {

	@Override
	protected GenerateXML getAction() {
		return new GenerateXML();
	}

	@Override
	protected FluentDefaultWidgetTest getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(FluentDefaultWidgetTest.MODULE_NAME, FluentDefaultWidgetTest.DOCUMENT_NAME);
	}
}