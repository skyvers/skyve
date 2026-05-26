package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.ControlPanel.ControlPanelExtension;

@SuppressWarnings("static-method")
public class DeleteTestDataTest {

	@Test
	void executeWithNullTestTagNameThrowsValidationException() {
		DeleteTestData action = new DeleteTestData();
		ControlPanelExtension bean = new ControlPanelExtension();
		// testTagName is null → throws before calling CORE
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}
}
