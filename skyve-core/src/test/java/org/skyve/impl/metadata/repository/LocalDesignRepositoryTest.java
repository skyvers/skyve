package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;

class LocalDesignRepositoryTest {

	@Test
	void getImplementingTypeForGenerateDomainValidationReturnsTypeWhenResolvable() {
		Attribute attribute = mock(Attribute.class);
		doReturn(String.class).when(attribute).getImplementingType();

		assertEquals(String.class, LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	@Test
	void getImplementingTypeForGenerateDomainValidationReturnsNullForUnresolvedEnumeration() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum class is not generated yet")).when(enumeration).getImplementingType();

		assertNull(LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	void getImplementingTypeForGenerateDomainValidationThrowsForUnresolvedNonEnumeration() {
		Attribute attribute = mock(Attribute.class);
		doThrow(new MetaDataException("Any non-enum metadata error")).when(attribute).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> LocalDesignRepository.getImplementingTypeForGenerateDomainValidation(attribute));
	}
}
