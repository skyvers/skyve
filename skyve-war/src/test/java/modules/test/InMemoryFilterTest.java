package modules.test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.collections.Predicate;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.types.Decimal2;
import org.skyve.metadata.view.model.list.InMemoryFilter;

class InMemoryFilterTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void testFilterStrings() {
		List<Bean> beans = new ArrayList<>(4);

		Map<String, Object> map = new TreeMap<>();
		map.put("name", "Ted");
		DynamicBean bean = new DynamicBean("admin", "Contact", map);
		beans.add(bean);

		map = new TreeMap<>();
		map.put("name", "Fred");
		bean = new DynamicBean("admin", "Contact", map);
		beans.add(bean);

		map = new TreeMap<>();
		map.put("name", "Jed");
		bean = new DynamicBean("admin", "Contact", map);
		beans.add(bean);

		map = new TreeMap<>();
		map.put("name", "Ned");
		bean = new DynamicBean("admin", "Contact", map);
		beans.add(bean);

		map = new TreeMap<>();
		map.put("name", null);
		bean = new DynamicBean("admin", "Contact", map);
		beans.add(bean);

		InMemoryFilter f = new InMemoryFilter();
		f.addStartsWith("name", "Je");

		f.filter(beans);
		Assert.assertEquals("Filter result is wrong", 1, beans.size());
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void testPredicateToStringWithValue() throws Exception {
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("name", "Ted");

		Field predicatesField = InMemoryFilter.class.getDeclaredField("predicates");
		predicatesField.setAccessible(true);
		List<Predicate> predicates = (List<Predicate>) predicatesField.get(f);

		String result = predicates.get(0).toString();
		Assert.assertTrue("toString should contain binding", result.contains("name"));
		Assert.assertTrue("toString should contain value", result.contains("Ted"));
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void testPredicateToStringWithStartAndEnd() throws Exception {
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("name", "A", "Z");

		Field predicatesField = InMemoryFilter.class.getDeclaredField("predicates");
		predicatesField.setAccessible(true);
		List<Predicate> predicates = (List<Predicate>) predicatesField.get(f);

		String result = predicates.get(0).toString();
		Assert.assertTrue("toString should contain binding", result.contains("name"));
		Assert.assertTrue("toString should contain 'and'", result.contains("and"));
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void testPredicateToStringWithNoValueOrRange() throws Exception {
		InMemoryFilter f = new InMemoryFilter();
		f.addNull("name");

		Field predicatesField = InMemoryFilter.class.getDeclaredField("predicates");
		predicatesField.setAccessible(true);
		List<Predicate> predicates = (List<Predicate>) predicatesField.get(f);

		String result = predicates.get(0).toString();
		Assert.assertTrue("toString should contain binding", result.contains("name"));
		Assert.assertTrue("toString should contain operator description", result.contains("is null"));
	}

	private static List<Bean> beansWithValue(String binding, Object value) {
		List<Bean> beans = new ArrayList<>(1);
		Map<String, Object> map = new TreeMap<>();
		map.put(binding, value);
		beans.add(new DynamicBean("admin", "Contact", map));
		return beans;
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNullRetainsNullBean() {
		List<Bean> beans = beansWithValue("name", null);
		InMemoryFilter f = new InMemoryFilter();
		f.addNull("name");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNullRemovesNonNullBean() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addNull("name");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotNullRetainsNonNullBean() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotNull("name");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotNullRemovesNullBean() {
		List<Bean> beans = beansWithValue("name", null);
		InMemoryFilter f = new InMemoryFilter();
		f.addNotNull("name");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsStringMatches() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("name", "Ted");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsStringNoMatch() {
		List<Bean> beans = beansWithValue("name", "Fred");
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("name", "Ted");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsIntegerMatches() {
		List<Bean> beans = beansWithValue("age", Integer.valueOf(42));
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("age", Integer.valueOf(42));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsLongMatches() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(100L));
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("count", Long.valueOf(100L));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsBooleanMatches() {
		List<Bean> beans = beansWithValue("active", Boolean.TRUE);
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("active", Boolean.TRUE);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsDecimalMatches() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("10.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("amount", new Decimal2("10.00"));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsStringRemovesMatch() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("name", "Ted");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsStringRetainsNonMatch() {
		List<Bean> beans = beansWithValue("name", "Fred");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("name", "Ted");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterContainsMatches() {
		List<Bean> beans = beansWithValue("name", "Frederick");
		InMemoryFilter f = new InMemoryFilter();
		f.addContains("name", "eder");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterContainsNoMatch() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addContains("name", "xyz");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotContainsRemovesMatch() {
		List<Bean> beans = beansWithValue("name", "Frederick");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotContains("name", "eder");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEndsWithMatches() {
		List<Bean> beans = beansWithValue("name", "Frederick");
		InMemoryFilter f = new InMemoryFilter();
		f.addEndsWith("name", "rick");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotStartsWithRemovesMatch() {
		List<Bean> beans = beansWithValue("name", "Frederick");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotStartsWith("name", "Fred");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEndsWithRemovesMatch() {
		List<Bean> beans = beansWithValue("name", "Frederick");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEndsWith("name", "rick");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanStringMatches() {
		List<Bean> beans = beansWithValue("name", "Z");
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThan("name", "A");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanIntegerMatches() {
		List<Bean> beans = beansWithValue("age", Integer.valueOf(50));
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThan("age", Integer.valueOf(40));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanLongMatches() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(200L));
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThan("count", Long.valueOf(100L));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanDecimalMatches() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("20.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThan("amount", new Decimal2("10.00"));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanOrEqualToStringMatches() {
		List<Bean> beans = beansWithValue("name", "B");
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThanOrEqualTo("name", "B");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanOrEqualToIntegerMatches() {
		List<Bean> beans = beansWithValue("age", Integer.valueOf(40));
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThanOrEqualTo("age", Integer.valueOf(40));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanOrEqualToLongMatches() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(100L));
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThanOrEqualTo("count", Long.valueOf(100L));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanOrEqualToDecimalMatches() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("10.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThanOrEqualTo("amount", new Decimal2("10.00"));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings({"static-method", "java:S5976"})
	void filterBetweenStringMatches() {
		List<Bean> beans = beansWithValue("name", "M");
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("name", "A", "Z");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterBetweenIntegerMatches() {
		List<Bean> beans = beansWithValue("age", Integer.valueOf(25));
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("age", Integer.valueOf(20), Integer.valueOf(30));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterBetweenLongMatches() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(150L));
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("count", Long.valueOf(100L), Long.valueOf(200L));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterBetweenDecimalMatches() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("15.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("amount", new Decimal2("10.00"), new Decimal2("20.00"));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsIgnoreCaseMatches() {
		List<Bean> beans = beansWithValue("name", "ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addEqualsIgnoreCase("name", "TED");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsIgnoreCaseRemovesMatch() {
		List<Bean> beans = beansWithValue("name", "ted");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEqualsIgnoreCase("name", "TED");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAndCombinesPredicates() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f1 = new InMemoryFilter();
		f1.addEquals("name", "Ted");
		InMemoryFilter f2 = new InMemoryFilter();
		f2.addNotNull("name");
		f1.addAnd(f2);
		f1.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterOrCombinesPredicates() {
		List<Bean> beans = beansWithValue("name", "Ted");
		InMemoryFilter f1 = new InMemoryFilter();
		f1.addEquals("name", "Fred");
		InMemoryFilter f2 = new InMemoryFilter();
		f2.addEquals("name", "Ted");
		f1.addOr(f2);
		f1.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsDateMatches() {
		Date d = new Date(0L);
		List<Bean> beans = beansWithValue("created", d);
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("created", d);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsIntegerRemovesMatch() {
		List<Bean> beans = beansWithValue("age", Integer.valueOf(42));
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("age", Integer.valueOf(42));
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsLongRemovesMatch() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(100L));
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("count", Long.valueOf(100L));
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsBooleanRemovesMatch() {
		List<Bean> beans = beansWithValue("active", Boolean.TRUE);
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("active", Boolean.TRUE);
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsDecimalRemovesMatch() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("10.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("amount", new Decimal2("10.00"));
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	// ---- addStartsWith positive ----

	@Test
	@SuppressWarnings("static-method")
	void filterStartsWithMatches() {
		List<Bean> beans = beansWithValue("name", "Hello");
		InMemoryFilter f = new InMemoryFilter();
		f.addStartsWith("name", "He");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterStartsWithNoMatch() {
		List<Bean> beans = beansWithValue("name", "Hello");
		InMemoryFilter f = new InMemoryFilter();
		f.addStartsWith("name", "Wo");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	// ---- addLessThan ----

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanStringMatches() {
		List<Bean> beans = beansWithValue("name", "Apple");
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThan("name", "Banana");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanStringNoMatch() {
		List<Bean> beans = beansWithValue("name", "Zebra");
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThan("name", "Apple");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanIntegerMatches() {
		List<Bean> beans = beansWithValue("count", Integer.valueOf(5));
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThan("count", Integer.valueOf(10));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanLongMatches() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(5L));
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThan("count", Long.valueOf(10L));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanDecimalMatches() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("5.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThan("amount", new Decimal2("10.00"));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanDateMatches() {
		Date earlier = new Date(1000L);
		Date later = new Date(2000L);
		List<Bean> beans = beansWithValue("when", earlier);
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThan("when", later);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	// ---- addLessThanOrEqualTo ----

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanOrEqualToStringMatches() {
		List<Bean> beans = beansWithValue("name", "Apple");
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThanOrEqualTo("name", "Apple");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanOrEqualToIntegerMatches() {
		List<Bean> beans = beansWithValue("count", Integer.valueOf(10));
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThanOrEqualTo("count", Integer.valueOf(10));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanOrEqualToLongMatches() {
		List<Bean> beans = beansWithValue("count", Long.valueOf(10L));
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThanOrEqualTo("count", Long.valueOf(10L));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanOrEqualToDecimalMatches() {
		List<Bean> beans = beansWithValue("amount", new Decimal2("10.00"));
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThanOrEqualTo("amount", new Decimal2("10.00"));
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterLessThanOrEqualToDateMatches() {
		Date d = new Date(1000L);
		List<Bean> beans = beansWithValue("when", d);
		InMemoryFilter f = new InMemoryFilter();
		f.addLessThanOrEqualTo("when", d);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	// ---- addGreaterThan(Date) and addGreaterThanOrEqualTo(Date) ----

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanDateMatches() {
		Date earlier = new Date(1000L);
		Date later = new Date(2000L);
		List<Bean> beans = beansWithValue("when", later);
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThan("when", earlier);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterGreaterThanOrEqualToDateMatches() {
		Date d = new Date(1000L);
		List<Bean> beans = beansWithValue("when", d);
		InMemoryFilter f = new InMemoryFilter();
		f.addGreaterThanOrEqualTo("when", d);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	// ---- addBetween(Date) ----

	@Test
	@SuppressWarnings("static-method")
	void filterBetweenDateMatches() {
		Date start = new Date(1000L);
		Date middle = new Date(1500L);
		Date end = new Date(2000L);
		List<Bean> beans = beansWithValue("when", middle);
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("when", start, end);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterBetweenDateNoMatch() {
		Date start = new Date(1000L);
		Date end = new Date(2000L);
		Date outside = new Date(3000L);
		List<Bean> beans = beansWithValue("when", outside);
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("when", start, end);
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	// ---- addIn ----

	@Test
	@SuppressWarnings("static-method")
	void filterInMatchesOneOfValues() {
		List<Bean> beans = beansWithValue("name", "Bob");
		InMemoryFilter f = new InMemoryFilter();
		f.addIn("name", "Alice", "Bob", "Charlie");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterInNoMatch() {
		List<Bean> beans = beansWithValue("name", "Dave");
		InMemoryFilter f = new InMemoryFilter();
		f.addIn("name", "Alice", "Bob", "Charlie");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterInWithNullBeanValueNoMatch() {
		List<Bean> beans = beansWithValue("name", null);
		InMemoryFilter f = new InMemoryFilter();
		f.addIn("name", "Alice", "Bob");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	// ---- addEquals(Enum) / addNotEquals(Enum) ----

	enum TestEnum { A, B, C }

	@Test
	@SuppressWarnings("static-method")
	void filterEqualsEnumMatches() {
		List<Bean> beans = beansWithValue("status", TestEnum.A);
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("status", TestEnum.A);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotEqualsEnumRemovesMatch() {
		List<Bean> beans = beansWithValue("status", TestEnum.A);
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("status", TestEnum.A);
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	// ---- reset ----

	@Test
	@SuppressWarnings("static-method")
	void filterNotInKeepsBeanNotInList() {
		List<Bean> beans = beansWithValue("name", "Alice");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotIn("name", "Bob", "Charlie");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterNotInRemovesBeanInList() {
		List<Bean> beans = beansWithValue("name", "Alice");
		InMemoryFilter f = new InMemoryFilter();
		f.addNotIn("name", "Alice", "Bob");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	// ---- geometry spatial filters ----

	@Test
	@SuppressWarnings("static-method")
	void filterWithinRetainsBeanInsidePolygon() {
		GeometryFactory gf = new GeometryFactory();
		Geometry point = gf.createPoint(new Coordinate(1.0, 1.0));
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("location", point);
		InMemoryFilter f = new InMemoryFilter();
		f.addWithin("location", polygon);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterWithinRemovesBeanOutsidePolygon() {
		GeometryFactory gf = new GeometryFactory();
		Geometry point = gf.createPoint(new Coordinate(5.0, 5.0));
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("location", point);
		InMemoryFilter f = new InMemoryFilter();
		f.addWithin("location", polygon);
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterContainsGeometryRetainsBeanContainingPoint() {
		GeometryFactory gf = new GeometryFactory();
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		Geometry point = gf.createPoint(new Coordinate(1.0, 1.0));
		List<Bean> beans = beansWithValue("area", polygon);
		InMemoryFilter f = new InMemoryFilter();
		f.addContains("area", point);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterDisjointRetainsBeanDisjointFromGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry point = gf.createPoint(new Coordinate(10.0, 10.0));
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("location", point);
		InMemoryFilter f = new InMemoryFilter();
		f.addDisjoint("location", polygon);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterIntersectsRetainsBeanIntersectingGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry line = gf.createLineString(new Coordinate[] {
			new Coordinate(0, 1), new Coordinate(2, 1)
		});
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("path", line);
		InMemoryFilter f = new InMemoryFilter();
		f.addIntersects("path", polygon);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterTouchesRetainsBeanTouchingGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry point = gf.createPoint(new Coordinate(0.0, 0.0));
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("location", point);
		InMemoryFilter f = new InMemoryFilter();
		f.addTouches("location", polygon);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterCrossesRetainsBeanCrossingGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry line = gf.createLineString(new Coordinate[] {
			new Coordinate(-1, 1), new Coordinate(3, 1)
		});
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("path", line);
		InMemoryFilter f = new InMemoryFilter();
		f.addCrosses("path", polygon);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterWithinRemovesNullBean() {
		GeometryFactory gf = new GeometryFactory();
		Geometry polygon = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		List<Bean> beans = beansWithValue("location", null);
		InMemoryFilter f = new InMemoryFilter();
		f.addWithin("location", polygon);
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterOverlapsRetainsBeanOverlappingGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry poly1 = gf.createPolygon(new Coordinate[] {
			new Coordinate(0, 0), new Coordinate(0, 2), new Coordinate(2, 2),
			new Coordinate(2, 0), new Coordinate(0, 0)
		});
		Geometry poly2 = gf.createPolygon(new Coordinate[] {
			new Coordinate(1, 0), new Coordinate(1, 2), new Coordinate(3, 2),
			new Coordinate(3, 0), new Coordinate(1, 0)
		});
		List<Bean> beans = beansWithValue("area", poly1);
		InMemoryFilter f = new InMemoryFilter();
		f.addOverlaps("area", poly2);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void isEmptyReturnsTrueForNewFilter() {
		InMemoryFilter f = new InMemoryFilter();
		Assert.assertTrue(f.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void isEmptyReturnsFalseAfterAddingPredicate() {
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("name", "Ted");
		Assert.assertFalse(f.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddNotEqualsDateRetainsBeanWithDifferentDate() {
		Date d1 = new Date(1000L);
		Date d2 = new Date(2000L);
		List<Bean> beans = beansWithValue("date", d1);
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("date", d2);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddNotEqualsDateRemovesBeanWithSameDate() {
		Date d = new Date(1000L);
		List<Bean> beans = beansWithValue("date", d);
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("date", d);
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddEqualsGeometryRetainsBeanWithMatchingGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry pt = gf.createPoint(new Coordinate(1, 1));
		List<Bean> beans = beansWithValue("location", pt);
		InMemoryFilter f = new InMemoryFilter();
		f.addEquals("location", pt);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddNotEqualsGeometryRetainsBeanWithDifferentGeometry() {
		GeometryFactory gf = new GeometryFactory();
		Geometry pt1 = gf.createPoint(new Coordinate(1, 1));
		Geometry pt2 = gf.createPoint(new Coordinate(2, 2));
		List<Bean> beans = beansWithValue("location", pt1);
		InMemoryFilter f = new InMemoryFilter();
		f.addNotEquals("location", pt2);
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddBetweenStringRetainsBeanInRange() {
		List<Bean> beans = beansWithValue("name", "Jane");
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("name", "A", "Z");
		f.filter(beans);
		Assert.assertEquals(1, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddBetweenStringRemovesBeanOutsideRange() {
		List<Bean> beans = beansWithValue("name", "Zebra");
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("name", "A", "M");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void filterAddBetweenStringReturnsFalseForNullBeanValue() {
		List<Bean> beans = beansWithValue("name", null);
		InMemoryFilter f = new InMemoryFilter();
		f.addBetween("name", "A", "Z");
		f.filter(beans);
		Assert.assertEquals(0, beans.size());
	}
}
