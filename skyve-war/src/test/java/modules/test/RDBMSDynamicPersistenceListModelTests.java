package modules.test;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

public class RDBMSDynamicPersistenceListModelTests extends AbstractSkyveTest {
	@Test
	public void testQuery() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		MetaDataQueryDefinition q = m.getMetaDataQuery("qRDBMSDynamic");
		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>();
		model.setQuery(q);
		
		model.getRows();
		// TODO check rows
	}

	@Test
	public void testModel() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		RDBMSDynamicPersistenceListModel<Bean> model = new RDBMSDynamicPersistenceListModel<>();
		
		List<MetaDataQueryColumn> columns = new ArrayList<>(1);
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(AllAttributesPersistent.colourPropertyName);
		column.setDisplayName("Colour");
		columns.add(column);
		
		model.setModel("Test", aadpd, columns);
		
		model.getRows();
		// TODO check rows
	}
}
