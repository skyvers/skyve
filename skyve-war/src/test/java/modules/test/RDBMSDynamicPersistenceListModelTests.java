package modules.test;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

class RDBMSDynamicPersistenceListModelTests extends AbstractSkyveTest {
	@Test
	void testConstructorQuery() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		MetaDataQueryDefinition q = m.getNullSafeMetaDataQuery("qRDBMSDynamic");
		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>(q);
		model.postConstruct(c, true);
		
		List<Bean> rows = model.getRows();
		Assertions.assertEquals(14, rows.size(), "Rows not the right size");
	}

	@Test
	void testPostConstructQuery() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		MetaDataQueryDefinition q = m.getNullSafeMetaDataQuery("qRDBMSDynamic");

		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>() {
			@Override
			public void postConstruct(Customer customer, boolean runtime) {
				setQuery(customer, q);
				super.postConstruct(customer, runtime);
			}
		};
		model.postConstruct(c, true);
		
		List<Bean> rows = model.getRows();
		Assertions.assertEquals(14, rows.size(), "Rows not the right size");
	}

	@Test
	void testSetBeanQuery() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		MetaDataQueryDefinition q = m.getNullSafeMetaDataQuery("qRDBMSDynamic");

		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>() {
			@Override
			public void setBean(Bean bean) {
				setQuery(c, q);
				super.setBean(bean);
			}
		};
		model.postConstruct(c, true);
		model.setBean(null);
		
		List<Bean> rows = model.getRows();
		Assertions.assertEquals(14, rows.size(), "Rows not the right size");
	}

	@Test
	void testConstructorModel() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		List<MetaDataQueryColumn> columns = new ArrayList<>(1);
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(AllAttributesPersistent.colourPropertyName);
		column.setDisplayName("Colour");
		columns.add(column);
		
		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>("Test", aadpd, columns);
		model.postConstruct(c, true);
		
		List<Bean> rows = model.getRows();
		Assertions.assertEquals(14, rows.size(), "Rows not the right size");
	}

	@Test
	void testPostConstructModel() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		List<MetaDataQueryColumn> columns = new ArrayList<>(1);
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(AllAttributesPersistent.colourPropertyName);
		column.setDisplayName("Colour");
		columns.add(column);
		
		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>() {
			@Override
			public void postConstruct(Customer customer, boolean runtime) {
				setModel(customer, "Test", aadpd, columns);
				super.postConstruct(customer, runtime);
			}
		};
		model.postConstruct(c, true);
		
		List<Bean> rows = model.getRows();
		Assertions.assertEquals(14, rows.size(), "Rows not the right size");
	}

	@Test
	void testSetBeanModel() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		List<MetaDataQueryColumn> columns = new ArrayList<>(1);
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(AllAttributesPersistent.colourPropertyName);
		column.setDisplayName("Colour");
		columns.add(column);
		
		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>() {
			@Override
			public void setBean(Bean bean) {
				setModel(c, "Test", aadpd, columns);
				super.setBean(bean);
			}
		};
		model.postConstruct(c, true);
		model.setBean(null);
		
		List<Bean> rows = model.getRows();
		Assertions.assertEquals(14, rows.size(), "Rows not the right size");
	}
}
