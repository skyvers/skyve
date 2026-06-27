package modules.admin.Tag;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.tag.TagManager;

import jakarta.enterprise.inject.Default;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient TagService tagService;
 */
@Default
public class TagService {
	/**
	 * Adds all objects from one tag into another tag.
	 *
	 * @param subject The tag receiving additional memberships.
	 * @param object The tag supplying memberships.
	 * @throws Exception If iteration or tagging fails.
	 */
	@SuppressWarnings("static-method")
	public void union(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			// no dialect insensitive way to use SQL for creating new UUIDs for new records
			// add tagged items from object tag to subject tag
			// TagManager function deals with duplicates
			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(object.getBizId())) {
				for (Bean bean : i) {
					tm.tag(subject.getBizId(), bean);
				}
			}
			subject.setUploadTagged(
					Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Keeps only memberships that appear in both subject and object tags.
	 *
	 * @param subject The tag being reduced.
	 * @param object The reference tag supplying required memberships.
	 * @throws Exception If SQL execution fails.
	 */
	@SuppressWarnings("static-method")
	public void intersect(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			Persistence pers = CORE.getPersistence();

			// unsecured SQL for performance
			StringBuilder intersect = new StringBuilder();
			intersect.append("delete from ADM_Tagged ");
			intersect.append(" where taggedBizId not in (");
			intersect.append(" select taggedBizId from ADM_Tagged where ");
			intersect.append(" tag_id = :objectTagId");
			intersect.append(" and bizCustomer=:objectBizCustomer").append(" ) ");
			intersect.append(" and tag_id =:subjectTagId");
			intersect.append(" and bizCustomer=:subjectBizCustomer");

			SQL sql = pers.newSQL(intersect.toString());
			sql.putParameter("objectTagId", object.getBizId(), false);
			sql.putParameter("subjectTagId", subject.getBizId(), false);
			sql.putParameter("objectBizCustomer", subject.getBizCustomer(), false);
			sql.putParameter("subjectBizCustomer", subject.getBizCustomer(), false);

			sql.execute();

			subject.setUploadTagged(
					Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Removes memberships from subject when the same beans are present in object.
	 *
	 * @param subject The tag being reduced.
	 * @param object The tag whose memberships should be subtracted.
	 * @throws Exception If iteration or untagging fails.
	 */
	@SuppressWarnings("static-method")
	public void except(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(object.getBizId())) {
				for (Bean bean : i) {
					// TagManager method handles if this bean was not tagged
					tm.untag(subject.getBizId(), bean);
				}
			}
			subject.setUploadTagged(
					Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Returns tagged beans constrained to a specific module and document.
	 *
	 * @param tag The tag whose beans are queried.
	 * @param moduleName The required module name.
	 * @param documentName The required document name.
	 * @return Matching tagged beans, or an empty list when filters are incomplete.
	 * @throws Exception If tag iteration fails.
	 */
	@SuppressWarnings("static-method")
	public List<Bean> getTaggedItemsForDocument(TagExtension tag, String moduleName, String documentName) throws Exception {
		List<Bean> beans = new ArrayList<>();
		if (moduleName != null && documentName != null) {
			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			if (tag != null) {
				try (AutoClosingIterable<Bean> i = EXT.getTagManager().iterate(tag.getBizId())) {
					for (Bean bean : i) {
						if (bean != null && bean.getBizModule().equals(module.getName())
								&& bean.getBizDocument().equals(document.getName())) {
							// need to check that this is only done for documents of the selected type
							beans.add(bean);
						}
					}
				}
			}
		}

		return beans;
	}
}
