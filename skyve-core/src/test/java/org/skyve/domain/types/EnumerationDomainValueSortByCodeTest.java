package org.skyve.domain.types;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Enumeration.DomainValueSortByCode;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

public class EnumerationDomainValueSortByCodeTest {

	@Test
	@SuppressWarnings("static-method")
	public void compareReturnsNegativeWhenFirstCodeIsLess() {
		DomainValueSortByCode comparator = new DomainValueSortByCode();
		DomainValue a = new DomainValue("a", "Alpha");
		DomainValue b = new DomainValue("b", "Beta");
		assertTrue(comparator.compare(a, b) < 0);
	}

	@Test
	@SuppressWarnings("static-method")
	public void compareReturnsPositiveWhenFirstCodeIsGreater() {
		DomainValueSortByCode comparator = new DomainValueSortByCode();
		DomainValue a = new DomainValue("z", "Zulu");
		DomainValue b = new DomainValue("a", "Alpha");
		assertTrue(comparator.compare(a, b) > 0);
	}

	@Test
	@SuppressWarnings("static-method")
	public void compareReturnsZeroWhenCodesEqual() {
		DomainValueSortByCode comparator = new DomainValueSortByCode();
		DomainValue a = new DomainValue("x", "One");
		DomainValue b = new DomainValue("x", "Two");
		assertTrue(comparator.compare(a, b) == 0);
	}

	@Test
	@SuppressWarnings("static-method")
	public void sortListByCode() {
		List<DomainValue> list = new ArrayList<>();
		list.add(new DomainValue("c", "C"));
		list.add(new DomainValue("a", "A"));
		list.add(new DomainValue("b", "B"));
		list.sort(new DomainValueSortByCode());
		assertTrue(list.get(0).getCode().equals("a"));
		assertTrue(list.get(1).getCode().equals("b"));
		assertTrue(list.get(2).getCode().equals("c"));
	}
}
