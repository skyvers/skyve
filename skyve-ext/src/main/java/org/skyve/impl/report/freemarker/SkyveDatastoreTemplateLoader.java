package org.skyve.impl.report.freemarker;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.admin.ReportTemplate;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;

import freemarker.cache.TemplateLoader;
import freemarker.template.TemplateNotFoundException;

public class SkyveDatastoreTemplateLoader implements TemplateLoader {
	/**
	 * Retrieves the associated template for a given id.
	 *
	 * When FreeMarker calls this function it appends a locale trying to find a specific
	 * version of a file. For example, if we need to retrieve the layout with name = "testReport", then
	 * FreeMarker will first try to load layoutId = testReport_en_US, followed by testReport_en and
	 * finally name = testReport.
	 * That's the reason why we have to catch NumberFormatException
	 * even if it is comes from a numeric field in the database.
	 *
	 * @param layoutId
	 * @return a template instance or null if not found.
	 * @throws IOException if a severe error happens, like not being
	 *         able to access the database.
	 */
	@Override
	public Object findTemplateSource(String name) throws IOException {
		// find the template with the specified name
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		q.getFilter().addEquals(ReportTemplate.templateNamePropertyName, name);
		q.getFilter().addEquals(ReportTemplate.enabledPropertyName, Boolean.TRUE);
		return q.beanResult();
	}

	/**
	 * Returns the last modification date of a given template.
	 * If the item does not exist anymore in the database, this method will return
	 * Long's MAX_VALUE to avoid FreeMarker recompiling the one in its cache.
	 */
	@Override
	public long getLastModified(Object templateSource) {
		// retrieve the bizlock
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		q.getFilter().addEquals(Bean.DOCUMENT_ID, ((Bean) templateSource).getBizId());
		q.addBoundProjection(PersistentBean.LOCK_NAME);
		OptimisticLock bizLock = q.scalarResult(OptimisticLock.class);

		if (bizLock != null) {
			return bizLock.getTimestamp().toInstant().toEpochMilli();
		}
		return Long.MAX_VALUE;
	}

	/**
	 * Returns a Reader from a template living in Freemarker's cache.
	 */
	@Override
	public Reader getReader(Object templateSource, String encoding) throws IOException {
		return new StringReader(((ReportTemplate) templateSource).getTemplate());
	}

	@Override
	public void closeTemplateSource(Object templateSource) throws IOException {
		// no action required
	}

	/**
	 * Description of this template loader, used by any {@link TemplateNotFoundException}s.
	 */
	@Override
	public String toString() {
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(ReportTemplate.MODULE_NAME);
		Document d = m.getDocument(c, ReportTemplate.DOCUMENT_NAME);
		Persistent p = d.getPersistent();
		String pid = (p == null) ? null : p.getPersistentIdentifier();

		StringBuilder result = new StringBuilder(128);
		result.append(this.getClass().getSimpleName()).append(" (user: ").append(u.getName());
		result.append(", table: ").append(pid).append(')');
		return result.toString();
	}
}
