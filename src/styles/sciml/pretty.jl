struct SciMLStyle <: AbstractStyle
    innerstyle::AbstractStyle
end
SciMLStyle() = SciMLStyle(NoopStyle())
getstyle(s::SciMLStyle) = s.innerstyle isa NoopStyle ? s : s.innerstyle

function options(::SciMLStyle)
    return (;
        always_for_in = true,
        always_use_return = false,
        annotate_untyped_fields_with_any = true,
        conditional_to_if = false,
        import_to_using = false,
        join_lines_based_on_source = true,
        normalize_line_endings = "unix",
        pipe_to_function_call = false,
        remove_extra_newlines = true,
        short_to_long_function_def = true,
        long_to_short_function_def = false,
        whitespace_in_kwargs = true,
        whitespace_ops_in_indices = true,
        whitespace_typedefs = true,
        indent = 4,
        margin = 92,
        format_docstrings = false,
        align_struct_field = false,
        align_assignment = false,
        align_conditional = false,
        align_pair_arrow = false,
        align_matrix = false,
        trailing_comma = false,
        trailing_zero = true,
        indent_submodule = false,
        separate_kwargs_with_semicolon = false,
        surround_whereop_typeparameters = true,
        variable_call_indent = [],
        yas_style_nesting = false,
        disallow_single_arg_nesting = true,
    )
end

function force_binaryop_whitespace(
    ::SciMLStyle,
    opkind::JuliaSyntax.Kind,
    ctx::PrettyContext,
    lineage::Vector{Tuple{JuliaSyntax.Kind,Bool,Bool}},
)
    !ctx.nospace &&
        !(ctx.from_ref || ctx.from_colon) &&
        !any(x -> x[1] === K"macrocall", lineage) &&
        opkind in KSet"= + - * / % && ||"
end
function force_no_unaryop_whitespace(
    ::SciMLStyle,
    opkind::JuliaSyntax.Kind,
    childs::AbstractVector{<:JuliaSyntax.GreenNode},
    i::Int,
)
    opkind in KSet"+ -" || return false

    next_idx = findnext(n -> !JuliaSyntax.is_whitespace(n), childs, i + 1)
    next_idx === nothing && return false
    return kind(childs[next_idx]) in KSet"Integer Float Float32"
end

function is_binaryop_nestable(::SciMLStyle, cst::JuliaSyntax.GreenNode)
    if (defines_function(cst) || is_assignment(cst))
        return false
    else
        false
    end
    !(op_kind(cst) in KSet"=> -> in")
end
@doc """
    SciMLStyle()

Formatting style based on [SciMLStyle](https://github.com/SciML/SciMLStyle).

Configurable options with different defaults to [`DefaultStyle`](@ref) are:
$(list_different_defaults(SciMLStyle()))
"""
SciMLStyle

const CST_T = [JuliaSyntax.GreenNode]
const TUPLE_T = [JuliaSyntax.GreenNode, Vector{JuliaSyntax.GreenNode}]
for f in [
    :p_import,
    :p_using,
    :p_export,
    :p_public,
    :p_vcat,
    :p_ncat,
    :p_typedvcat,
    :p_typedncat,
    :p_row,
    :p_nrow,
    :p_hcat,
    :p_comprehension,
    :p_typedcomprehension,
    :p_generator,
    :p_filter,
]
    @eval function $f(
        ss::SciMLStyle,
        cst::JuliaSyntax.GreenNode,
        s::State,
        ctx::PrettyContext,
        lineage::Vector{Tuple{JuliaSyntax.Kind,Bool,Bool}},
    )
        $f(YASStyle(getstyle(ss)), cst, s, ctx, lineage)
    end
end

for f in [
    :p_curly,
    :p_ref,
    :p_braces,
    # :p_vect, don't use YAS style vector formatting with `yas_style_nesting = true`
    :p_parameters,
    :p_invisbrackets,
    :p_bracescat,
]
    @eval function $f(
        ss::SciMLStyle,
        cst::JuliaSyntax.GreenNode,
        s::State,
        ctx::PrettyContext,
        lineage::Vector{Tuple{JuliaSyntax.Kind,Bool,Bool}},
    )
        if s.opts.yas_style_nesting
            $f(YASStyle(getstyle(ss)), cst, s, ctx, lineage)
        else
            $f(DefaultStyle(getstyle(ss)), cst, s, ctx, lineage)
        end
    end
end

function _empty_named_tuple_call(fst::FST, s::State)
    if fst.typ === TupleN &&
       length(fst.nodes::Vector) == 3 &&
       fst[1].typ === PUNCTUATION &&
       fst[1].val == "(" &&
       fst[2].typ === SEMICOLON &&
       fst[3].typ === PUNCTUATION &&
       fst[3].val == ")"
        t = FST(Call, fst.indent)
        add_node!(
            t,
            FST(IDENTIFIER, fst[1].line_offset, fst.startline, fst.startline, "NamedTuple"),
            s;
            join_lines = true,
        )
        add_node!(
            t,
            FST(PUNCTUATION, -1, fst.startline, fst.startline, "("),
            s;
            join_lines = true,
        )
        add_node!(
            t,
            FST(PUNCTUATION, -1, fst.startline, fst.startline, ")"),
            s;
            join_lines = true,
        )
        return t
    end
    return fst
end

function _separate_positional_kwargs_with_semicolon!(fst::FST)
    nodes = fst.nodes::Vector{FST}
    kw_idx = findfirst(n -> n.typ === Kw, nodes)
    kw_idx === nothing && return
    any(is_comma, nodes[1:(kw_idx-1)]) || return

    separate_kwargs_with_semicolon!(fst)
end

function p_call(
    ss::SciMLStyle,
    cst::JuliaSyntax.GreenNode,
    s::State,
    ctx::PrettyContext,
    lineage::Vector{Tuple{JuliaSyntax.Kind,Bool,Bool}},
)
    style = getstyle(ss)
    t = if s.opts.yas_style_nesting
        p_call(YASStyle(style), cst, s, ctx, lineage)
    else
        p_call(DefaultStyle(style), cst, s, ctx, lineage)
    end
    ctx.can_separate_kwargs && _separate_positional_kwargs_with_semicolon!(t)
    return t
end

function p_tuple(
    ss::SciMLStyle,
    cst::JuliaSyntax.GreenNode,
    s::State,
    ctx::PrettyContext,
    lineage::Vector{Tuple{JuliaSyntax.Kind,Bool,Bool}},
)
    t = if s.opts.yas_style_nesting
        p_tuple(YASStyle(getstyle(ss)), cst, s, ctx, lineage)
    else
        p_tuple(DefaultStyle(getstyle(ss)), cst, s, ctx, lineage)
    end
    return _empty_named_tuple_call(t, s)
end

function p_macrocall(
    ss::SciMLStyle,
    cst::JuliaSyntax.GreenNode,
    s::State,
    ctx::PrettyContext,
    lineage::Vector{Tuple{JuliaSyntax.Kind,Bool,Bool}},
)
    style = getstyle(ss)
    t = FST(MacroCall, nspaces(s))
    if !haschildren(cst)
        return t
    end

    childs = children(cst)
    has_closer = is_closer(childs[end])
    is_macroblock = !has_closer

    if is_macroblock
        t.typ = MacroBlock
    end

    idx = findfirst(a -> kind(a) === K"(", childs)
    first_arg_idx =
        idx === nothing ? -1 : findnext(a -> !JuliaSyntax.is_whitespace(a), childs, idx + 1)

    # https://github.com/SciML/SciMLStyle?tab=readme-ov-file#macros
    n_kw_args = count(a -> kind(a) === K"=" && haschildren(a), childs)
    nospace = n_kw_args > 1

    for (i, a) in enumerate(childs)
        n = pretty(
            style,
            a,
            s,
            newctx(ctx; nospace = nospace, can_separate_kwargs = false),
            lineage,
        )::FST

        override = (i == first_arg_idx) || kind(a) === K")"

        if JuliaSyntax.is_macro_name(a) || kind(a) === K"("
            add_node!(t, n, s; join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, n, s; join_lines = true)
        elseif is_macroblock
            if n.typ === MacroBlock && t[end].typ === WHITESPACE
                t[end] = Placeholder(length(t[end].val))
            end

            max_padding = is_block(n) ? 0 : -1
            join_lines = t.endline == n.startline

            if join_lines && (i > 1 && kind(childs[i-1]) in KSet"NewlineWs Whitespace") ||
               next_node_is(nn -> kind(nn) in KSet"NewlineWs Whitespace", childs[i])
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s; join_lines, max_padding)
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        end
    end
    # move placement of @ to the end
    #
    # @Module.macro -> Module.@macro
    t[1] = move_at_sign_to_the_end(t[1], s)
    t
end
