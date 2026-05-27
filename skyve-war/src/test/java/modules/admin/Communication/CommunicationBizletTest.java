package modules.admin.Communication;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.domain.messages.ValidationException;

import modules.admin.domain.Communication;

@SuppressWarnings("static-method")
class CommunicationBizletTest {

	private static final CommunicationBizlet bizlet = new CommunicationBizlet();

	@Test
	void checkForUnsavedDataWithNoOriginalValuesDoesNotThrow() {
		CommunicationExtension bean = new CommunicationExtension();
		assertDoesNotThrow(() -> CommunicationBizlet.checkForUnsavedData(bean));
	}

	@Test
	void preRerenderWithModuleNameSourceClearsDocumentName() throws Exception {
		CommunicationExtension bean = new CommunicationExtension();
		bean.setDocumentName("someDocument");
		bizlet.preRerender(Communication.moduleNamePropertyName, bean, null);
		assertNull(bean.getDocumentName());
	}

	@Test
	void preRerenderWithUnknownSourceDoesNothing() throws Exception {
		CommunicationExtension bean = Assertions.assertDoesNotThrow(CommunicationExtension::new);
		bean.setDocumentName("someDocument");
		bizlet.preRerender("unknownSource", bean, null);
		// document name should be unchanged
	}

	@Test
	void preDeleteWithUnlockedBeanDoesNotThrow() {
		CommunicationExtension bean = new CommunicationExtension();
		// locked is false by default
		assertDoesNotThrow(() -> bizlet.preDelete(bean));
	}

	@Test
	void preDeleteWithLockedBeanThrowsValidationException() {
		CommunicationExtension bean = new CommunicationExtension();
		// isLocked() = isPersisted() && systemUse=true
		bean.setBizVersion(Integer.valueOf(1));
		bean.setSystemUse(Boolean.TRUE);
		assertThrows(ValidationException.class, () -> bizlet.preDelete(bean));
	}
}
