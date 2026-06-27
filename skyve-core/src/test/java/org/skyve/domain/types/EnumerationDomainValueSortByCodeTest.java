package org.skyve.domain.types;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Enumeration.DomainValueSortByCode;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

class EnumerationDomainValueSortByCodeTest {

	@Test
	@SuppressWarnings("static-method")
	void compareReturnsNegativeWhenFirstCodeIsLess() {
		DomainValueSortByCode comparator = new DomainValueSortByCode();
		DomainValue a = new DomainValue("a", "Alpha");
		DomainValue b = new DomainValue("b", "Beta");
		assertTrue(comparator.compare(a, b) < 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void compareReturnsPositiveWhenFirstCodeIsGreater() {
		DomainValueSortByCode comparator = new DomainValueSortByCode();
		DomainValue a = new DomainValue("z", "Zulu");
		DomainValue b = new DomainValue("a", "Alpha");
		assertTrue(comparator.compare(a, b) > 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void compareReturnsZeroWhenCodesEqual() {
		DomainValueSortByCode comparator = new DomainValueSortByCode();
		DomainValue a = new DomainValue("x", "One");
		DomainValue b = new DomainValue("x", "Two");
		assertEquals(0, comparator.compare(a, b));
	}

	@Test
	@SuppressWarnings("static-method")
	void sortListByCode() {
		List<DomainValue> list = new ArrayList<>();
		list.add(new DomainValue("c", "C"));
		list.add(new DomainValue("a", "A"));
		list.add(new DomainValue("b", "B"));
		list.sort(new DomainValueSortByCode());
		assertEquals(list.get(0).getCode(), "a");
		assertEquals(list.get(1).getCode(), "b");
		assertEquals(list.get(2).getCode(), "c");
	}
}
