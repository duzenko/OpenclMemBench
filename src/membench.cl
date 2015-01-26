__kernel void somekernel(__global int4 *dst)
{
        int no = get_global_id(0);
        dst[no] = (int4)123;
}
