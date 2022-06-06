package modules.test;

import org.junit.Test;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.util.Util;

public class RDBMSDynamicPersistenceListModelTests extends AbstractSkyveTest {
	@Test
	public void test() throws Exception {
		DynamicPersistentBean row = Util.constructRandomInstance(u, m, aadpd, 2);
		row = p.save(row);
		row = Util.constructRandomInstance(u, m, aadpd, 2);
		p.save(row);
		
		MetaDataQueryDefinition q = m.getMetaDataQuery("qRDBMSDynamic");
		RDBMSDynamicPersistenceListModel<DynamicPersistentBean> model = new RDBMSDynamicPersistenceListModel<>();
		model.setQuery(q);
		
		model.getRows();
	}
}
