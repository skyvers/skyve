package modules.testModule.domain;

import java.util.ArrayList;
import java.util.List;

public class OwnerDoc extends org.skyve.impl.domain.AbstractPersistentBean {
	private static final long serialVersionUID = 1L;

	private String text;
	private RelatedDoc association;
	private final List<RelatedDoc> children = new ArrayList<>();

	@Override
	public String getBizKey() {
		return getBizId();
	}

	@Override
	public String getBizModule() {
		return "testModule";
	}

	@Override
	public String getBizDocument() {
		return "OwnerDoc";
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public RelatedDoc getAssociation() {
		return association;
	}

	public void setAssociation(RelatedDoc association) {
		this.association = association;
	}

	public List<RelatedDoc> getChildren() {
		return children;
	}
}
