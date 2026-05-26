package modules.admin.DocumentCreator.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.domain.DocumentCreator;

@SuppressWarnings("static-method")
public class SubmitActionTest {

	@Test
	void executeWithNullOutputLocationThrowsValidationException() {
		Submit action = new Submit();
		DocumentCreator bean = new DocumentCreator();
		// outputLocation is null → throws before doing anything complex
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}
}
