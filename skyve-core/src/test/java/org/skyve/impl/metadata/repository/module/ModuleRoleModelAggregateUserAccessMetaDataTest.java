package org.skyve.impl.metadata.repository.module;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;

class ModuleRoleModelAggregateUserAccessMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	void validateThrowsWhenModelNameIsNull() {
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Map<String, DocumentRef> refs = Collections.singletonMap("MyDocument", ref);
		when(module.getDocumentRefs()).thenReturn(refs);
		when(module.getName()).thenReturn("TestModule");

		ModuleRoleModelAggregateUserAccessMetaData metaData = new ModuleRoleModelAggregateUserAccessMetaData();
		metaData.setDocumentName("MyDocument");
		// modelName left null

		assertThrows(MetaDataException.class, () -> metaData.validate("TestMeta", "TestRole", module));
	}

	@Test
	@SuppressWarnings("static-method")
	void validatePassesWhenAllFieldsSet() {
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Map<String, DocumentRef> refs = Collections.singletonMap("MyDocument", ref);
		when(module.getDocumentRefs()).thenReturn(refs);
		when(module.getName()).thenReturn("TestModule");

		ModuleRoleModelAggregateUserAccessMetaData metaData = new ModuleRoleModelAggregateUserAccessMetaData();
		metaData.setDocumentName("MyDocument");
		metaData.setModelName("MyModel");

		// Should not throw
		assertDoesNotThrow(() -> metaData.validate("TestMeta", "TestRole", module));
	}
}
