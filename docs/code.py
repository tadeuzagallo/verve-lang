from docutils import nodes
from docutils.parsers.rst import Directive, directives

class code_node(nodes.Structural, nodes.Element):
    pass

class CodeDirective(Directive):
    has_content = True
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False
    option_spec = {}

    def run(self):
        code = code_node()
        code['language'] = self.arguments[0]
        code['source'] = '\n'.join(self.content)
        return [code]

def visit_code_node(self, node):
    pass

def depart_code_node(self, node):
    klass = 'class="language-{}"'.format(node['language'])
    link = """<pre {0}><code {0}>{1}</code></pre>""".format(klass, node['source'])
    self.body.append(link)

def setup(app):
    app.add_directive('prismjs', CodeDirective)
    app.add_node(code_node, html=(visit_code_node, depart_code_node))
