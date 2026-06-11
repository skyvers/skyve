package modules.admin.Tag.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import modules.admin.Tag.TagExtension;

@SuppressWarnings("static-method")
class PrepareExplanationTest {

	@Test
	void executeWithNullOperandTagSetsCombinationExplanationToEmpty() throws Exception {
		PrepareExplanation action = new PrepareExplanation();
		TagExtension bean = new TagExtension();
		// operandTag is null, so explanation will be empty
		var result = action.execute(bean, null);
		assertNotNull(result);
		assertEquals("", bean.getCombinationExplanation());
	}
}
