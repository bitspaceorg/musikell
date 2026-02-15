import os
import frontmatter

REQUIRED_FIELDS = ["title", "description", "slug", "author", "date"]

def parse_file(file_path, base_url):
    with open(file_path, "r", encoding="utf-8") as f:
        post = frontmatter.load(f)

    for field in REQUIRED_FIELDS:
        if field not in post.metadata or post.metadata.get(field) in [None, ""]:
            raise ValueError(f"Missing required field '{field}' in {file_path}")

    return {
        "title": post.metadata["title"],
        "description": post.metadata["description"],
        "url": f"{base_url}/{file_path}",
        "slug": post.metadata["slug"],
        "author": post.metadata["author"],
        "date": post.metadata["date"],
        "tags": post.metadata.get("tags", []),
    }


def collect_docs():
    docs = []
    authors = set()
    tags = set()
    slugs = set()
    base_url = "https://gitlab.com/bitspaceorg/claire/musikell/-/raw/main"

    entries = os.listdir("docs")
    mdx_files = {e[:-4] for e in entries if e.endswith(".mdx")}
    dirs = {e for e in entries if os.path.isdir(os.path.join("docs", e))}

    for name in sorted(mdx_files):
        file_path = os.path.join("docs", f"{name}.mdx")
        doc = parse_file(file_path, base_url)

        if doc["slug"] in slugs:
            raise ValueError(f"Duplicate slug: {doc['slug']}")
        slugs.add(doc["slug"])

        authors.add(doc["author"])
        tags.update(doc["tags"])

        if name in dirs:
            children = []
            child_dir = os.path.join("docs", name)

            for file in os.listdir(child_dir):
                if file.endswith(".mdx"):
                    child_path = os.path.join(child_dir, file)
                    child_doc = parse_file(child_path, base_url)

                    if child_doc["slug"] in slugs:
                        raise ValueError(f"Duplicate slug: {child_doc['slug']}")
                    slugs.add(child_doc["slug"])

                    children.append(child_doc)
                    authors.add(child_doc["author"])
                    tags.update(child_doc["tags"])

            if children:
                doc["children"] = children

        docs.append(doc)

    return docs, sorted(authors), sorted(tags)

