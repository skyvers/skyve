package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.persistence.DocumentQuery;

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
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qH").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
	}

	@Test
	public void testHierarchicalPolymorphic() {
		Assert.assertTrue((((AbstractQuery) m.getDocumentQuery("qHPoly").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
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
		Assert.assertTrue((((AbstractQuery) m.getDocumentQuery("qMEJS").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
	}

	@Test
	public void testMEJSNotPolymorphic() {
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMEJSNotPoly").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
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
		Assert.assertTrue((((AbstractQuery) m.getDocumentQuery("qMESS").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
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
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMSJS").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
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
		Assert.assertFalse((((AbstractQuery) m.getDocumentQuery("qMSSS").constructDocumentQuery(null, null)).toQueryString().contains("bean as bean")));
	}
}
