package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Util;

import modules.test.domain.MappedSubclassedSingleStrategy;
import util.AbstractH2Test;

public class DocumentQueryTest extends AbstractH2Test {
	@Test
	public void testDefaultHierarchical() {
		DocumentQuery q = m.getDocumentDefaultQuery(c, hd.getName()).constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testHierarchical() {
		DocumentQuery q = m.getDocumentQuery("qH").constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testHierarchicalNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qH").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testHierarchicalPolymorphic() {
		Assert.assertTrue((((AbstractQuery) m.getDocumentQuery("qHPoly").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testAggregateHierarchicalNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qH").constructDocumentQuery(AggregateFunction.Count, null))
				.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testAggregateHierarchicalPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qHPoly").constructDocumentQuery(AggregateFunction.Count, null))
				.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testDefaultMEJS() {
		DocumentQuery q = m.getDocumentDefaultQuery(c, mejsd.getName()).constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMEJS() {
		DocumentQuery q = m.getDocumentQuery("qMEJS").constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMEJSPolymorphic() {
		Assert.assertTrue((((AbstractQuery) m.getDocumentQuery("qMEJS").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testMEJSNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMEJSNotPoly").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testAggregateMEJSPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMEJS").constructDocumentQuery(AggregateFunction.Count, null))
				.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testAggregateMEJSNotPolymorphic() {
		Assert.assertFalse(
				(((AbstractQuery) m.getDocumentQuery("qMEJSNotPoly").constructDocumentQuery(AggregateFunction.Count, null))
						.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testDefaultMESS() {
		DocumentQuery q = m.getDocumentDefaultQuery(c, messd.getName()).constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMESS() {
		DocumentQuery q = m.getDocumentQuery("qMESS").constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMESSPolymorphic() {
		Assert.assertTrue((((AbstractQuery) m.getDocumentQuery("qMESS").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testAggregateMESSPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMESS").constructDocumentQuery(AggregateFunction.Count, null))
				.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testDefaultMSJS() {
		DocumentQuery q = m.getDocumentDefaultQuery(c, msjsd.getName()).constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMSJS() {
		DocumentQuery q = m.getDocumentQuery("qMSJS").constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMSJSNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMSJS").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testAggregateMSJSNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMSJS").constructDocumentQuery(AggregateFunction.Count, null))
				.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testDefaultMSSS() {
		DocumentQuery q = m.getDocumentDefaultQuery(c, msssd.getName()).constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMSSS() {
		DocumentQuery q = m.getDocumentQuery("qMSSS").constructDocumentQuery(null, null);
		q.tupleResults();
	}

	@Test
	public void testMSSSNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMSSS").constructDocumentQuery(null, null)).toQueryString()
				.contains("bean as bean")));
	}

	@Test
	public void testAggregateMSSSNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMSSS").constructDocumentQuery(AggregateFunction.Count, null))
				.toQueryString().contains("bean as bean")));
	}

	@Test
	public void testQueryColumnBindingToNeither() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 1);
		test = p.save(test);

		// If binding evaluates to null, then no filter criteria is added
		Assert.assertEquals(1,
				m.getDocumentQuery("qMetaDataQueryColumnBinding").constructDocumentQuery(null, null).projectedResults().size());
	}

	@Test
	public void testQueryColumnBindingToStash() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 1);
		test.setText("ICAL");
		test = p.save(test);

		CORE.getStash().put("TEST", "ICAL");
		Assert.assertEquals(1,
				m.getDocumentQuery("qMetaDataQueryColumnBinding").constructDocumentQuery(null, null).projectedResults().size());
	}

	@Test
	public void testQueryColumnBindingToUserAttributes() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 1);
		test.setText("ICAL");
		test = p.save(test);

		CORE.getUser().getAttributes().put("TEST", "ICAL");
		Assert.assertEquals(1,
				m.getDocumentQuery("qMetaDataQueryColumnBinding").constructDocumentQuery(null, null).projectedResults().size());
	}

	@Test
	public void testQueryFromAndFilterBindingToNeither() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 1);
		test = p.save(test);

		// If binding evaluates to null, null is bound to a query parameter
		Assert.assertEquals(0, m.getDocumentQuery("qMetaDataQueryFromAndFilterBinding").constructDocumentQuery(null, null)
				.projectedResults().size());
	}

	@Test
	public void testQueryFromAndFilterBindingToStash() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 1);
		test.setText("ICAL");
		test = p.save(test);

		CORE.getStash().put("TEST", "ICAL");
		Assert.assertEquals(1, m.getDocumentQuery("qMetaDataQueryFromAndFilterBinding").constructDocumentQuery(null, null)
				.projectedResults().size());
	}

	@Test
	public void testQueryFromAndFilterBindingToUserAttributes() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 1);
		test.setText("ICAL");
		test = p.save(test);

		CORE.getUser().getAttributes().put("TEST", "ICAL");
		Assert.assertEquals(1, m.getDocumentQuery("qMetaDataQueryFromAndFilterBinding").constructDocumentQuery(null, null)
				.projectedResults().size());
	}
}
