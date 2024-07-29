package modules.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import modules.admin.Group.GroupExtension;
import modules.admin.domain.Group;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.Hierarchical;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class BizPortTest extends AbstractSkyveTest {
	@Test
	public void testStandardSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 3);
		test = p.save(test);
		bizport(test);
	}

	// test transient object
	
	@Test
	public void testStandardJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 3);
		test = p.save(test);
		bizport(test);
	}

	@Test
	public void testStandardSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 3);
		test = p.save(test);
		bizport(test);
	}

	@Test
	public void testStandardSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 3);
		test = p.save(test);
		bizport(test);
	}

	@Test
	public void testStandardChild() throws Exception {
		org.skyve.metadata.module.Module admin = c.getModule(Group.MODULE_NAME);
		Document document = admin.getDocument(c, Group.DOCUMENT_NAME);
		GroupExtension group = Util.constructRandomInstance(u, admin, document, 3);
		group = p.save(group);
		bizport(group);
	}
	
	@Test
	public void testStandardEmbedded() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		bizport(test);
	}
	
	@Test
	public void testStandardHierarchical() throws Exception {
		Hierarchical root = Util.constructRandomInstance(u, m, hd, 1);
		Hierarchical child = Util.constructRandomInstance(u, m, hd, 1);
		child.setBizParentId(root.getBizId());
		Hierarchical grandChild = Util.constructRandomInstance(u, m, hd, 1);
		grandChild.setBizParentId(child.getBizId());
		
		root = p.save(root);
		child = p.save(child);
		grandChild = p.save(grandChild);
		bizport(root);
	}
	
	private void bizport(Bean bean) throws Exception {
		BizPortWorkbook workbook = EXT.standardBizExport(bean);
// Uncomment to put the xlsx to the file system
//try (OutputStream os = new FileOutputStream("/Users/mike/Downloads/test.xlsx")) {
//workbook.write(os);
//os.flush();
//}
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(10240)) {
			workbook.write(baos);
			byte[] bytes = baos.toByteArray();
			try (ByteArrayInputStream bais = new ByteArrayInputStream(bytes)) {
				UploadException problems = new UploadException();
				EXT.standardBizImport(bais, problems);
				
				Bean importedBean = CORE.getPersistence().retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());
				Assert.assertTrue("Beans not the same", same(bean, importedBean));
			}
			catch (UploadException e ) {
				handleUploadException(e);
			}
		}
	}
	
	private static void handleUploadException(UploadException e) {
		StringBuilder sb = new StringBuilder(1024);
		for (Problem p : e.getWarnings()) {
			sb.append(p.toString()).append('\n');
		}
		for (Problem p : e.getErrors()) {
			sb.append(p.toString()).append('\n');
		}
		Assert.fail(sb.toString());
	}
	
	private boolean same(@Nonnull Bean one, @Nonnull Bean other) {
		Set<String> samesame = new TreeSet<>();
		return same(one, other, samesame);
	}
	
	private boolean same(@Nonnull Bean one, @Nonnull Bean other, @Nonnull Set<String> samesame) {
		String samesameKey = one.getBizId() +" = " + other.getBizId();
		if (samesame.contains(samesameKey)) { // already tested
			return true;
		}
		samesame.add(samesameKey);
		
		try {
			Document document = one.getDocumentMetaData();
			for (Attribute a : document.getAllAttributes(c)) {
				String name = a.getName();
				Object oneValue = Binder.get(one, name);
				Object otherValue = Binder.get(other, name);
				if (oneValue == null) {
					if (otherValue != null) {
						return false;
					}
				}
				else {
					if (otherValue == null) {
						return false;
					}
	
					if (oneValue instanceof Bean) {
						if (otherValue instanceof Bean) {
							if (! same((Bean) oneValue, (Bean) otherValue, samesame)) {
								return false;
							}
						}
						else {
							return false;
						}
					}
					else if (oneValue instanceof List<?>) {
						if (otherValue instanceof List<?>) {
							@SuppressWarnings("unchecked")
							List<Bean> oneList = (List<Bean>) oneValue;
							@SuppressWarnings("unchecked")
							List<Bean> otherList = (List<Bean>) otherValue;
							if (oneList.size() != otherList.size()) {
								return false;
							}
							for (int i = 0, l = oneList.size(); i < l; i++) {
								if (! same(oneList.get(i), otherList.get(i), samesame)) {
									return false;
								}
							}
						}
						else {
							return false;
						}
					}
					else {
						if (! oneValue.equals(otherValue)) {
							return false;
						}
					}
				}
			}
			return true;
		}
		catch (@SuppressWarnings("unused") MetaDataException e) {
			return false;
		}
	}
}
